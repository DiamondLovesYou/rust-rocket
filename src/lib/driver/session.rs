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
use std::io;
use std::comm;
use std::comm::{Chan, Port};

/// Sessions:
/// FIXME(diamond): 

/// Interfaces:
/// FIXME(diamond): add supervisors.
/// FIXME(diamond): macro this.

/// These interfaces below serve to make our session object light enough
/// to be pragmatically copy-able.

enum CMIMessage {
    AddCMIRefMsg,
    DropCMIRefMsg,

    GetCodeMapCMIMessage(Chan<codemap::CodeMap>),
}
pub struct CodeMapperInterface {
    chan: Chan<CMIMessage>,
}
impl CodeMapperInterface {
    fn new() -> CodeMapperInterface {
        let (port, chan) = comm::Chan::new();
        spawn(proc() {
                let port = port;
                let mut codemap = codemap::CodeMap::new();
                for message in port.iter() {
                    match message {
                        QuitCMIMessage(ret) => {
                            ret.send(());
                            break;
                        }
                        GetCodeMapCMIMessage(ret) => ret.try_send(codemap.clone()),
                    }
                }
            });
        CodeMapperInterface {
            chan: chan,
        }
    }

    pub fn full_codemap(&self) -> Future<codemap::CodeMap> {
        let (port, chan) = comm::Chan::new();
        self.chan.send(GetCodeMapCMIMessage(chan));
        Future::from_port(port)
    }
}

enum UserConfigMessage {
    QuitUCMessage(comm::Port<()>),
}
pub struct UserConfigInterface {
    chan: comm::Chan<UserConfigMessage>,
}
impl drop::Drop for UserConfigInterface {
    fn drop(&mut self) {
        let (port, chan) = Chan::new();
        if self.chan.try_send(QuitCMIMessage(chan)) {
            port.recv_opt();
        }
    }
}
enum BackendLoggerMessage {
    AddBLIfRefMsg,
    DropBLIfRefMsg,
}
/// a logger used in the backend task for non-fatal messages (mostly just for warnings).
pub struct BackendLoggerIf {
    chan: Chan<BackendLoggerMessage>
}
pub enum StaleDataset<TSet> {
    /// if the db version in the file doesn't match FRESHNESS_DB_VERSION,
    /// we need to unconditionally treat all input files as fresh even
    /// though, because we can't read the db, we have no clue which specific
    /// files are stale. This will also happen if the db was corrupted.
    AllStaleDataset,
    SpecificStaleDataset(TSet),
}
impl<TMap: container::Map<Path, TMV>, TMV > StaleDataset {
    pub fn is_stale(&self, path: &Path) -> bool {
        match self {
            &AllStaleDataset => true,
            &SpecificStaleDataset(ref map) => map.contains_key(path),
        }
    }
}
impl<TSet: container::Set<Path>> StaleDataset {
    pub fn is_stale(&self, path: &Path) -> bool {
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

    QueryStalenessMsg(Chan<IoTimeStampResultDataMap>, HashSet<Path>),
    /// for when we only need a yes or no answer.
    QueryAnyFreshnessMsg(Chan<bool>, HashSet<Path>),
    QueryIndividualFreshnessMsg(Chan<HashMap<Path, bool>>, HashSet<Path>),
    TrackFreshnessMsg(HashMap<Path, TrackingMeta>),

    /// you should almost always prefer to signal the individual files as fresh
    /// instead of this 'nuke from orbit' option.
    RefreshFreshnessMsg,
    ///
    SignalFreshnessMsg(HashSet<Path>),
}
#[deriving(Clone, Eq, Encodable, Decodable)]
pub struct TrackingMeta {
    last_mod: u64,
    traverse_symlinks: bool,

    /// can we stop tracking this path if during a session
    /// it's stale and the router doesn't re-track it?
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
    fn find_stale_files(&self, logger: &BackendLoggerIf) -> Option<IoTimeStampResultDataMet> {
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
/// FreshnessIf responds to queries regarding the freshness state of tracked files.
/// Naturally, it also maintains the list of files we use to decide what needs to be built.
/// It is target independent due to the target independent nature of source files.
pub struct FreshnessIf {
    chan: Chan<FreshnessMessage>,
}
static FRESHNESS_DB_VERSION: uint = 0;
impl FreshnessIf {
    fn run_db(port: &Port<FreshnessMessage>,
              logger: &BackendLoggerIf,
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
                                .map(|p| stale_map.find_or_insert_with(p.clone(), ret_modified) )
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
                                stale_map.find_or_insert_with(p, ret_modified);
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
                                stale_map.find_or_insert_with(p, ret_modified);
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
                    new_tracking.extend(to_track.clone());
                    tracking_set.extend(to_track.map(|(p, _)| p));
                }
            }
        }

        // shutdown:
        let tracking = tracking_map
            .move_iter()
            .filter_map(|(p, meta)| {
                let newly_tracked = new_tracking.pop(p);
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
            .chain(new_tracking.move_iter())
            .collect();
        let db = FreshnessDb {
            tracking: tracking,
        };
        db.save(db_path);
        

        return ref_count != 0;
    }
    fn task(port: Port<FreshnessMessage>,
            logger: BackendLoggerIf,
            db_path: &Path) {
        while FreshnessIf::run_db(port, logger, db_path) {}
    }

    fn new(be_logger: BackendLoggerIf, db_path: Path) -> FreshnessIf {
        
    }

    pub fn is_fresh(&self, path: Path) -> Future<bool> {
        let mut set = HashSet::new();
        set.insert(path);
        self.are_any_fresh(set)
    }
    pub fn are_any_fresh(&self, paths: HashSet<Path>) -> Future<bool> {
        let (port, chan) = Chan::new();
        self.chan.send(QueryFreshnessMsg(chan, paths));
        Future::from_port(port)
    }
    pub fn individual_freshness(&self, paths: HashSet<Path>) -> Future<HashMap<Path, bool>> {
        let (port, chan) = Chan::new();
        self.chan.send(QueryIndividualFreshnessMsg(chan, paths));
        Future::from_port(port)
    }

    pub fn staleness(&self, paths: HashSet<Path>) -> Future<IoTimeStampResultDataMap> {
        let (port, chan) = Chan::new();
        self.chan.send(QueryStalenessMsg(chan, paths));
        Future::from_port(port)
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

pub trait Driver {
    fn fatal(&self, origin: Origin, msg: &str) -> !;
    fn err(&self, origin: Origin, msg: &str);
    fn warn(&self, origin: Origin, msg: &str);
    fn info(&self, origin: Origin, msg: &str);
    fn abort_if_errors(&self);
}

#[deriving(Clone)]
pub struct Session_ {
    codemapper: CodeMapperIf,
    freshness:  FreshnessIf,
    dep:        DepIf,
    workcache:  WorkCacheIf,
}
impl Session {
    pub fn codemapper<'a>(&'a self) -> &'a CodeMapperIf {
        self.codemapper
    }
    pub fn freshness<'a>(&'a self) -> &'a FreshnessIf {
        self.freshness
    }
    pub fn dep<'a>(&'a self) -> &'a DepIf {
        self.dep
    }
    pub fn workcache<'a>(&'a self) -> &'a WorkCacheIf {
        self.workcache
    }

    pub fn ensure_dir(&self, dir: &Path) {
        if dir.exists() {
            match io::fs::lstat(dir) {
                Ok(FileStat { perm: perm, .. }) if perm & io::UserRWX == io::UserRWX => {},
                Ok(FileStat { perm: perm, .. }) => {
                    match io::fs::chmod(dir, perm | io::UserRWX) {
                        Ok(()) => {},
                        Err(e) => {
                            self.fatal(format!("couldn't chmod directory {}: {}",
                                               dir.display(),
                                               e))
                        }
                    }
                }
                Err(e) => {
                    self.fatal(format!("couldn't lstat directory {}: {}",
                                       dir.display(),
                                       e))
                }
            }
        } else {
            match io::fs::mkdir_recursive(dir, io::UserRWX) {
                Ok(()) => {},
                Err(e) => {
                    self.fatal(format!("couldn't create directory {}: {}",
                                       dir.display(),
                                       e));
                }
            }
        }
    }
}
impl Driver for Session_ {
    fn fatal(&self, origin: Origin, msg: &str) -> ! {
        
    }
    fn err(&self, origin: Origin, msg: &str) {
    }
    fn warn(&self, origin: Origin, msg: &str) {
    }
    fn info(&self, origin: Origin, msg: &str) {
    }
}
#[deriving(Clone)]
pub struct Router {
    addr: address::Address,

    diag:   diagnostics::SessionIf,
    dep:    dep::SessionIf,
    wc:     workcache::SessionIf,
    build:  build_crate::SessionIf,
}
impl Router {
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
impl Session for Router {

    fn target_id(&self) -> TargetId {
        self.addr.target_id().expect("this address should always have a target prefix")
    }
    fn subtarget_id(&self) -> SubtargetId {
        self.addr.subtarget_id().expect("this address should always have a subtarget prefix")
    }
}

pub struct Configure {
    src_path: Path,
    build_path: Path,
    rustb_path: Path,
    
    default_target: Option<Target>,
}
pub struct Target {
    name: String,
    mach: mach_target::Target,
    
}
impl Target {
    pub fn new() -> Target {
        
    }
}
impl Default for Target {
    fn default() -> Target {
        Target {
            name: "default".to_string(),
            mach_target: mach_target::get_host_triple()
        }
    }
}

pub fn find_crates_in_dir<TSess: Session>(_sess: &TSess, cwd: &Path) -> io::IoResult<~[Crate]> {
    let mut contents = try!(io::fs::readdir(cwd));
    contents.retain(|p| -> bool {
            p != BUILD_CRATE_FILENAME && p.extension_str().map_or(false, |ext| ext == "rs")
        });
    result::Ok(contents.move_iter().filter_map(|p| {
            // FIXME(diamond): this could reasonably be done in parallel.
            task::try(proc() {
                    let cm = codemap::CodeMap::new();
                    let sh = mk_silent_span_handler(cm);
                    let parse_sess = ParseSess::new_special_handler_path(sh,
                                                                         cm,
                                                                         path);
                    let crate_config = Vec::new();
                    let attrs = parse_crate_attrs_from_file(p,
                                                            crate_config,
                                                            parse_sess);
                    attr::find_crateid(attrs)
                        .map(|id| {
                            Crate {
                                id: Some(id),
                                file: p.clone()
                            }
                        }).unwrap_or_else(|| {
                            Crate {
                                id: None,
                                file: p.clone()
                            }
                        })
                }).ok()
        }).collect())
}
