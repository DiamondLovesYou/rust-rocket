// Copyright (c) 2014 Richard Diamond & contributors.
//
// This file is part of Rust Rocket.
//
// Rust Rocket is free software: you can redistribute it and/or modify
// it under the terms of the GNU Lesser General Public License as published by
// the Free Software Foundation, either version 3 of the License, or
// (at your option) any later version.
//
// Rust Rocket is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU Lesser General Public License
// along with Rust Rocket. If not, see <http://www.gnu.org/licenses/>.

use std::path::Path;
use syntax::codemap;
use syntax::crateid::CrateId;
use std::io;
use std::comm;
use std::comm::{Receiver, Sender};
use std::collections;
use std::io::{fs, FileStat, IoResult};
use std::collections::{HashSet, HashMap};
use std::sync::Future;

use address::{Address, CratePhase};
use railroad::YardId;
use super::super::{TargetId, SubtargetId};

use super::diagnostics;
use super::{DiagIf, WorkCacheIf};

// Interfaces:
// FIXME(diamond): add supervisors.
// FIXME(diamond): macro this.

// These interfaces below serve to make our session object light enough
// to be pragmatically copy-able.

pub enum StaleDataset<TSet> {
    // if the db version in the file doesn't match FRESHNESS_DB_VERSION,
    // we need to unconditionally treat all input files as fresh even
    // though, because we can't read the db, we have no clue which specific
    // files are stale. This will also happen if the db was corrupted.
    AllStaleDataset,
    SpecificStaleDataset(TSet),
}
pub trait IsStale {
    fn is_stale(&self, path: &Path) -> bool;
}
impl<TMap: collections::Map<Path, TMV>, TMV> StaleDataset<TMap> {
    pub fn is_set_stale(&self, path: &Path) -> bool {
        match self {
            &AllStaleDataset => true,
            &SpecificStaleDataset(ref map) => map.contains_key(path),
        }
    }
}
impl<TSet: collections::Set<Path>> StaleDataset<TSet> {
    pub fn is_map_stale(&self, path: &Path) -> bool {
        match self {
            &AllStaleDataset => true,
            &SpecificStaleDataset(ref set) => set.contains(path),
        }
    }
}
type IoTimeStampResultDataMap = HashMap<Path, IoResult<u64>>;
enum FreshnessMessage {
    AddFreshIfRefMsg,
    DropFreshIfRefMsg,

    QueryStalenessMsg(Sender<IoTimeStampResultDataMap>, HashSet<Path>),
    // for when we only need a yes or no answer.
    QueryAnyFreshnessMsg(Sender<bool>, HashSet<Path>),
    QueryIndividualFreshnessMsg(Sender<HashMap<Path, bool>>, HashSet<Path>),
    TrackFreshnessMsg(HashMap<Path, TrackingMeta>),

    // you should almost always prefer to signal the individual files as fresh
    // instead of this 'nuke from orbit' option.
    RefreshFreshnessMsg,
    //
    SignalFreshnessMsg(HashSet<Path>),
}
#[deriving(Clone, Eq, Encodable, Decodable)]
pub struct TrackingMeta {
    last_mod: u64,
    traverse_symlinks: bool,

    // can we stop tracking this path if during a session
    // it's stale and the router doesn't re-track it?
    removable: bool,
}
#[deriving(Encodable, Decodable)]
struct FreshnessDb {
    tracking: HashMap<Path, TrackingMeta>,
}
impl FreshnessDb {
    fn load(path: &Path) -> Option<FreshnessDb> {
        
    }
    /// if the return is None, it means all files should be considered stale.
    fn find_stale_files(&self, logger: &DiagIf) -> Option<IoTimeStampResultDataMap> {
        if self.tracking.len() == 0 {
            None
        } else {
            Some(self
                 .tracking
                 .iter()
                 .map(|(path, tracked)| {
                     (path, match tracked.traverse_symlinks {
                         false => fs::lstat(path),
                         true  => fs::stat(path),
                     }, tracked.last_mod)
                 })
                 .filter_map(|(path, stat_result, old_ts)| {
                     (path, match stat_result {
                         Ok(FileStat { modified: modified, ..}) if modified > old_ts => {
                             Some(Ok(modified))
                         }
                         Ok(_) => None,
                         Err(err) => Some(Err(err)),
                     })
                 }).collect())
        }
    }
}
// FreshnessIf responds to queries regarding the freshness state of tracked files.
// Naturally, it also maintains the list of files we use to decide what needs to be built.
// It is target independent due to the target independent nature of source files.
pub struct FreshnessIf {
    chan: Sender<FreshnessMessage>,
}
static FRESHNESS_DB_VERSION: uint = 0;
impl FreshnessIf {
    fn run_db(port: &Receiver<FreshnessMessage>,
              logger: &DiagIf,
              db_path: &Path) -> bool {
        let (mut stale_map,
             mut dragnetting,
             mut tracking_map) = match FreshnessDb::load(logger, db_path) {
            Some(mut db) => match db.find_stale_files(logger) {
                Some(stale_map) => (stale_map, false, db.tracking),
                None => (HashMap::new(), true, db.tracking),
            },
            None => (HashMap::new(), true, HashMap::new()),
        };
        let tracking_set = tracking_map.keys().collect();
        let mut new_tracks = HashMap::new();
        let mut ref_count: u64 = 1;

        fn modified(p: &Path) -> IoResult<u64> {
            Ok(try!(fs::stat(p)).modified)
        }

        for msg in port.iter() {
            match msg {
                AddFreshIfRefMsg => ref_count += 1,
                DropFreshIfRefMsg => { ref_count -= 1; if ref_count == 0 { break; } },

                QueryStalenessMsg(ret, queried) => {
                    let result = match dragnetting {
                        true => {
                            queried
                                .move_iter()
                                .map(|p| stale_map.find_or_insert_with(p.clone(), modified) )
                                .collect()
                        }
                        false => {
                            let queried = tracking_set.intersection(queried);
                            queried
                                .move_iter()
                                .filter_map(|p| stale_map.find_equiv(p) )
                                .collect()
                        }
                    };
                    ret.try_send(result);
                }
                QueryAnyFreshnessMsg(ret, queried) => {
                    match dragnetting {
                        true => {
                            // send ret value immediately, then do processing.
                            ret.try_send(true);
                            for p in queried.move_iter() {
                                stale_map.find_or_insert_with(p, modified);
                            }
                        }
                        false => {
                            let queried = tracking_set.intersection(queried);
                            let result = queried
                                .move_iter()
                                .any(|p| stale_map.contains_key_equiv(p) );
                            ret.try_send(result);
                        }
                    }
                }
                QueryIndividualFreshnessMsg(ret, queried) => {
                    match dragnetting {
                        true => {
                            // send ret value immediately, then do processing.
                            ret.try_send(queried.iter().map(|p| (p, true) ).collect());
                            for p in queried.move_iter() {
                                stale_map.find_or_insert_with(p, modified);
                            }
                        }
                        false => {
                            let queried = tracking_set.intersection(queried);
                            let result = queried
                                .move_iter()
                                .map(|p| (p, stale_map.contains_key_equiv(p)) )
                                .collect();
                            ret.try_send(result);
                        }
                    }
                }

                TrackFreshnessMsg(to_track) => {
                    let to_track = to_track.move_iter();
                    new_tracks.extend(to_track.clone());
                    tracking_set.extend(to_track.map(|(p, _)| p));
                }
            }
        }

        // shutdown:
        let tracking = tracking_map
            .move_iter()
            .filter_map(|(p, meta)| {
                let newly_tracked = new_tracks.pop(p);
                if meta.removable {
                    newly_tracked.map(|meta| (p, meta) )
                } else {
                    Some((p,
                          TrackingMeta {
                                last_mod: match stale_map.pop(p) {
                                    Some(Ok(modified)) => modified,
                                    Some(Err(_)) | None => {
                                        // keep the old mod time
                                        meta.last_mod
                                    }
                                },
                                .. meta
                            }))
                }
            })
            .chain(new_tracks.move_iter())
            .collect();
        let db = FreshnessDb {
            tracking: tracking,
        };
        db.save(db_path);

        return ref_count != 0;
    }
    fn task(port: Receiver<FreshnessMessage>,
            diag: DiagIf,
            db_path: &Path) {
        while FreshnessIf::run_db(port, &diag, db_path) {}
    }

    fn new(diag: DiagIf, db_path: Path) -> FreshnessIf {
        
    }

    pub fn is_fresh(&self, path: Path) -> Future<bool> {
        let mut set = HashSet::new();
        set.insert(path);
        self.are_any_fresh(set)
    }
    pub fn are_any_fresh(&self, paths: HashSet<Path>) -> Future<bool> {
        let (port, chan) = channel();
        self.chan.send(QueryAnyFreshnessMsg(chan, paths));
        Future::from_receiver(port)
    }
    pub fn individual_freshness(&self, paths: HashSet<Path>) -> Future<HashMap<Path, bool>> {
        let (port, chan) = channel();
        self.chan.send(QueryIndividualFreshnessMsg(chan, paths));
        Future::from_receiver(port)
    }

    pub fn staleness(&self, paths: HashSet<Path>) -> Future<IoTimeStampResultDataMap> {
        let (port, chan) = channel();
        self.chan.send(QueryStalenessMsg(chan, paths));
        Future::from_receiver(port)
    }
    pub fn track_one(&self, path: Path, meta: TrackingMeta) {
        let mut paths = HashMap::new();
        paths.insert(path, meta);
        self.track_all(paths);
    }
    pub fn track_all(&self, paths: HashMap<Path, TrackingMeta>) {
        self.chan.send(TrackFreshnessMsg(paths));
    }
}

// Defines the interface to session data managed on a per target basis.
pub struct TargetIf;

#[deriving(Clone)]
pub struct Session {
    diag:       diagnostics::SessionIf,
    freshness:  FreshnessIf,
//    dep:        DepIf,
    workcache:  WorkCacheIf,
}
impl Session {
    pub fn freshness<'a>(&'a self) -> &'a FreshnessIf {
        self.freshness
    }
    /*pub fn dep<'a>(&'a self) -> &'a DepIf {
        self.dep
    }*/
    pub fn workcache<'a>(&'a self) -> &'a WorkCacheIf {
        self.workcache
    }
}

#[deriving(Clone)]
pub struct Router {
    addr: Address,

    diag:   diagnostics::SessionIf,
//    dep:    DepIf,
    wc:     WorkCacheIf,
    //build:  build_crate::SessionIf,
}
impl Router {
    pub fn address<'a>(&'a self) -> &'a Address { &self.address }

    pub fn incr_yard(&mut self, yard: YardId) {
        self.address = self.address.with_yard_suffex(yard);
    }
    // injects a dep to another yard.
    pub fn inject_crate_dep(&self, yard: YardId, dep: Address) {
        self.dep.inject_crate_dep(self.addr.clone(), yard, dep)
    }
    // reports a dep of this yard.
    pub fn report_crate_dep(&self, krate: CrateId, phase: CratePhase) {
        
    }
    pub fn report_fs_deps(&self, files: Vec<Path>) {
        self.dep.report_fs_deps(self.addr.clone(), files)
    }
}
