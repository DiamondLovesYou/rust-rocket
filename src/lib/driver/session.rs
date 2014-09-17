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
use super::{DiagIf, WorkcacheIf};

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

    // you should almost always prefer to signal individual files as fresh
    // instead of this 'nuke from orbit' option.
    RefreshFreshnessMsg,
    SignalFreshnessMsg(HashSet<Path>),
}
#[deriving(Clone, Eq, PartialEq, Encodable, Decodable)]
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
    fn load() -> Option<FreshnessDb> {

    }
    /// if the return is None, it means all files should be considered stale.
    fn find_stale_files(&self) -> Option<IoTimeStampResultDataMap> {
        if self.tracking.len() == 0 {
            None
        } else {
            Some(self.tracking
                 .iter()
                 .map(|(path, tracked)| {
                     (path, match tracked.traverse_symlinks {
                         false => fs::lstat(path),
                         true  => fs::stat(path),
                     }, tracked.last_mod)
                 })
                 .filter_map(|(path, stat_result, old_ts)| {
                     match stat_result {
                         Ok(FileStat { modified: modified, ..}) if modified > old_ts => {
                             Some((path.clone(), Ok(modified)))
                         }
                         Ok(_) => None,
                         Err(err) => Some((path.clone(), Err(err))),
                     }
                 })
                 .collect())
        }
    }
}
// FreshnessIf responds to queries regarding the freshness state of tracked files.
// Naturally, it also maintains the list of files we use to decide what needs to be built.
// It is target independent due to the target independent nature of source
// files.
#[deriving(Clone)]
pub struct FreshnessIf {
    chan: Sender<FreshnessMessage>,
}
static FRESHNESS_DB_VERSION: uint = 0;
impl FreshnessIf {
    fn run_db(port: &Receiver<FreshnessMessage>) -> bool {
        let (mut stale_map,
             mut dragnetting,
             mut tracking_map) = match FreshnessDb::load() {
            Some(mut db) => match db.find_stale_files() {
                Some(stale_map) => (stale_map, false, db.tracking),
                None => (HashMap::new(), true, db.tracking),
            },
            None => (HashMap::new(), true, HashMap::new()),
        };
        let tracking_set: HashSet<Path> = tracking_map
            .keys()
            .map(|k| k.clone() )
            .collect();
        let mut new_tracks: HashMap<Path, TrackingMeta> = HashMap::new();
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
                                .map(|p| (p, *stale_map.find_or_insert_with(p.clone(), modified)) )
                                .collect()
                        }
                        false => {
                            tracking_set
                                .intersection(&queried)
                                .filter_map(|p| {
                                    stale_map
                                        .find(p)
                                        .map(|r| (p.clone(), r.clone()))
                                })
                                .collect()
                        }
                    };
                    let _ = ret.send_opt(result);
                }
                QueryAnyFreshnessMsg(ret, queried) => {
                    match dragnetting {
                        true => {
                            // send ret value immediately, then do processing.
                            let _ = ret.send_opt(true);
                            for p in queried.move_iter() {
                                stale_map.find_or_insert_with(p, modified);
                            }
                        }
                        false => {
                            let result = tracking_set
                                .intersection(&queried)
                                .any(|p| stale_map.contains_key(p) );
                            let _ = ret.send_opt(result);
                        }
                    }
                }
                QueryIndividualFreshnessMsg(ret, queried) => {
                    match dragnetting {
                        true => {
                            // send ret value immediately, then do processing.
                            let _ = ret.send_opt(queried.iter().map(|p| (p.clone(), true) ).collect());
                            for p in queried.move_iter() {
                                stale_map.find_or_insert_with(p, modified);
                            }
                        }
                        false => {
                            let result = tracking_set
                                .intersection(&queried)
                                .map(|p| (p.clone(), stale_map.contains_key(p)) )
                                .collect();
                            let _ = ret.send_opt(result);
                        }
                    }
                }

                TrackFreshnessMsg(to_track) => {
                    for (p, meta) in to_track.move_iter() {
                        new_tracks.insert(p.clone(), meta.clone());
                        tracking_set.insert(p);
                    }
                }
            }
        }

        // shutdown:
        let tracking = tracking_map
            .move_iter()
            .filter_map(|(p, meta)| {
                let newly_tracked = new_tracks.pop(&p);
                if meta.removable {
                    newly_tracked.map(|meta| (p.clone(), meta.clone()) )
                } else {
                    Some((p,
                          TrackingMeta {
                                last_mod: match stale_map.pop(&p) {
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
        //db.save(db_path);

        return ref_count != 0;
    }
    fn task(port: Receiver<FreshnessMessage>,
            diag: DiagIf,
            db_path: &Path) {
        while FreshnessIf::run_db(&port) {}
    }

    fn new() -> FreshnessIf {

    }

    pub fn is_fresh(&self, path: Path) -> Future<bool> {
        let mut set = HashSet::new();
        set.insert(path);
        self.are_any_fresh(set)
    }
    pub fn are_any_fresh(&self, paths: HashSet<Path>) -> Future<bool> {
        let (s, r) = channel();
        self.chan.send(QueryAnyFreshnessMsg(s, paths));
        Future::from_receiver(r)
    }
    pub fn individual_freshness(&self, paths: HashSet<Path>) -> Future<HashMap<Path, bool>> {
        let (s, r) = channel();
        self.chan.send(QueryIndividualFreshnessMsg(s, paths));
        Future::from_receiver(r)
    }

    pub fn staleness(&self, paths: HashSet<Path>) -> Future<IoTimeStampResultDataMap> {
        let (s, r) = channel();
        self.chan.send(QueryStalenessMsg(s, paths));
        Future::from_receiver(r)
    }
    pub fn track_one(&self, path: Path, meta: TrackingMeta) {
        let mut paths = HashMap::new();
        paths.insert(path, meta);
        self.track_all(paths);
    }
    pub fn track_all(&self, paths: HashMap<Path, TrackingMeta>) {
        self.chan.send(TrackFreshnessMsg(paths));
    }

    // Inform the manager that the provided addresses were changed. This expects
    // only source prefix addresses.
    // IDE writers: if you'd like to preemptively compile the project (such as
    // before the user has explicitly saved a file), create a source file
    // override first then call this.
    pub fn signal_stale(&self, addrs: HashSet<Address>) {
        unimplemented!()
    }
}

#[deriving(Clone)]
pub struct Router {
    addr: Address,

    yard_to_id: HashMap<String, YardId>,
}
impl Router {

    pub fn resolve_yard(&self, name: &str) -> Option<YardId> {
        self.yard_to_id
            .find_equiv(&name.to_string())
            .map(|&c| c )
    }

    // injects a dep to another yard.
    pub fn inject_crate_dep(&self, yard: YardId, dep: Address) {
    }
    // reports a dep of this yard.
    pub fn report_crate_dep(&self, krate: CrateId, phase: CratePhase) {

    }
    pub fn report_fs_deps(&self, files: Vec<Path>) {
    }
}
