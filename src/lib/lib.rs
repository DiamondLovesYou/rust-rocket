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

#![crate_name = "rocket"]
#![comment = "A Rust project builder"]
#![license = "GNU/LGPLv3"]
#![crate_type = "dylib"]
#![crate_type = "rlib"]

#![feature(phase, managed_boxes, struct_variant, macro_rules)]
#![feature(default_type_params)]

extern crate syntax;
extern crate rustc;
extern crate std;
extern crate glob;
extern crate term;
extern crate arena;
extern crate serialize;
#[phase(plugin, link)]
extern crate log;
extern crate green;
extern crate sqlite = "sqlite3";
extern crate bindgen;
extern crate collections;
extern crate rustuv;
extern crate sync;
extern crate time;
extern crate url;
extern crate uuid;

use buildcrate::BuildCrate;

use syntax::ast::{CrateConfig, Name};
use syntax::{codemap, crateid};
use syntax::diagnostic::{Emitter, Handler, Level, SpanHandler};
use std::fmt;
use std::path::Path;
use std::str::MaybeOwned;
use std::rc::{Rc, Weak};
use rustc::driver::config::CrateType;

pub use rustc::driver::driver::host_triple;

use override::Origin;

pub static BUILD_CRATE_FILENAME: &'static str = "build.rs";
pub static ROCKET_PATH: &'static str = ".rocket";
pub static DB_FILENAME: &'static str = "master.db";
pub static DB_EXT: &'static str = "db";
pub static IN_TREE_BUILD_SUBPATH: &'static str = "build";
pub static TARGETS_PATH: &'static str = "target";

static BUILD_SUBTARGET_ID: SubtargetId = 0;
static BUILD_SUBTARGET_NAME: &'static str = "build";
static HOST_SUBTARGET_ID: SubtargetId = 1;
static HOST_SUBTARGET_NAME: &'static str = "host";
static TARGET_SUBTARGET_ID: SubtargetId = 2;
static TARGET_SUBTARGET_NAME: &'static str = "target";

macro_rules! build_session(
    (
        enum Message {
            $($msg:ident ($($bind:pat),*): ($($ty:ty),*) => $block:block),*
        }
        struct Database {
            version: $version:expr,

            $($db_col:ident : $db_col_ty:ty),*
        }
        struct Session {
            $($sess_col:ident : $sess_col_ty:ty),*
        }
    ) => (
        enum Message {
            AddRefMsg,
            // if Some(..), send back the post de-reference ref count.
            // used during shutdown to assert there are no more refs.
            DropRefMsg(Option<Sender<u64>>),

            $($msg ($($ty),*)),*
        }

        // Stores Stuff we don't want (or can't) save to disk.
        struct Session {
            db:   ::sqlite::Database,

            ref_count: uint,

            diag: driver::DiagIf,
        }

        impl Session {
            fn new_task(diag: driver::DiagIf,
                        rustb: Path) -> SessionIf {
                tasks::spawn(proc() {
                    
                })
            }
        }

        pub struct SessionIf {
            chan: Sender<Message>,
        }
        impl clone::Clone for SessionIf {
            fn clone(&self) -> SessionIf {
                self.chan.send(AddRefMsg);
                SessionIf {
                    chan: self.chan.clone();
                }
            }
        }
        impl drop::Drop for SessionIf {
            fn drop(&mut self) {
                self.chan.send(DropRefMsg(None));
            }
        }
    );
)

// These need to go after the build_session macro so they have access to it.
pub mod buildcrate;
pub mod toolchain;
pub mod user;
pub mod driver;
pub mod platform_dep;
pub mod address;
pub mod override;
pub mod workcache;
pub mod railroad;
//pub mod dep;
pub mod stats;

struct SilentEmitter;

impl Emitter for SilentEmitter {
    fn emit(&self,
            _cmsp: Option<(&codemap::CodeMap, codemap::Span)>,
            _msg: &str,
            _lvl: Level) {}
    fn custom_emit(&self,
                   _cm: &codemap::CodeMap,
                   _sp: codemap::Span,
                   _msg: &str,
                   _lvl: Level) {}
}

pub fn mk_silent_handler() -> Handler {
    use syntax::diagnostic::mk_handler;
    mk_handler(box SilentEmitter)
}
pub fn mk_silent_span_handler(cm: codemap::CodeMap) -> SpanHandler {
    SpanHandler {
        cm: cm,
        handler: mk_silent_handler(),
    }
}

static INITIAL_MSTR_DB: &'static [u8] = include_bin!("master.db");

fn create_initial_mstr_db(rustb_path: &Path) -> sqlite::Database {
    use std::io::{File, Open, ReadWrite};
    let mstr_db_f = rustb_path.join(DB_FILENAME);
    {
        let file = File::open_mode(&mstr_db_f, Open, ReadWrite).expect();
        file.write(INITIAL_MSTR_DB);
        file.flush();
    }
    sqlite::open(format!("file:{}", mstr_db_f))
            .expect("couldn't create sqlite db!")
}

#[deriving(Encodable, Decodable)]
pub struct Build {
    targets: Vec<Target>,

    src_path: Path,
    build_path: Path,
    
    build_crate: Box<buildcrate::Crate>,
    db: sqlite::Database,
    
    externs: Option<Vec<Extern>>,
}

impl Build {
    pub fn new(config: BuildConfigure) -> Build {
        let BuildConfigure {
            src_path,
            build_path,

            targets,
        } = config;

        let build = Build {
            src_path: src_path,
            build_path: build_path,

            targets: match targets {
                None => Vec::new(),
                Some(targets) => targets.move_iter()
                    .map(|target| {
                        Target {
                            id: 0,
                            name: target.name,

                            custom_subtargets: Vec::new(),
                            build_subtarget: target.build_triple.map(|build| {
                                Subtarget {
                                    id:   BUILD_SUBTARGET_ID,
                                    name: BUILD_SUBTARGET_NAME,

                                    triple: build,
                                }
                            }),
                            host_subtarget:  target.host_triple.map(|host| {
                                Subtarget {
                                    id:   HOST_SUBTARGET_ID,
                                    name: HOST_SUBTARGET_NAME,

                                    triple: host,
                                }
                            }),

                            triple: target.target_triple.unwrap_or_else(|| {
                                host_triple().to_string()
                            }),
                        }
                    })
                    .collect()
            }
        };

        let mut id = 0;
        for target in build.targets.mut_iter() {
            target.id = id;
            id += 1;
        }

        let db = create_initial_mstr_db(&config.rustb_path);
        assert!(db.exec(format!("INSERT INTO build (src_path) VALUES (\"{}\")",
                                config.src_path)).expect())

        build
    }

    pub fn load(path: &Path) -> Option<Build> {
        let db_path = path.join_many([ROCKET_PATH, DB_FILENAME]);
        
    }

    pub fn create_target(&mut self,
                         config: TargetConfigure) -> TargetId {
        let target = config.configure(self);

        self.ensure_dir(self.targets_path);
        self.ensure_dir(target.path);
        self.targets.push(target);
    }
    pub fn build(&mut self,
                 target: Vec<String>) {
        for t in self.targets
            .mut_iter()
            .filter(|t| target.is_none() || t.name == target.unwrap() ) {
            t.build(self);
        }
    }

    // ensures dir exists within the build dir.
    pub fn ensure_dir(&self, dir: &Path) {
        use std::io::fs::{lstat, chmod, mkdir_recursive};
        use std::io;
        use std::io::{FileStat};

        assert!(dir.is_relative());
        let dir = self.build_path.join(dir);
        if dir.exists() {
            match lstat(dir) {
                Ok(FileStat { perm: perm, .. }) if perm & io::UserRWX == io::UserRWX => {},
                Ok(FileStat { perm: perm, .. }) => {
                    match chmod(dir, perm | io::UserRWX) {
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
            match mkdir_recursive(dir, io::UserRWX) {
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
pub struct BuildConfigure {
    src_path: Path,
    build_path: Path,

    // Use Some with an empty Vec to configure no targets.
    targets: Option<Vec<TargetConfigure>>,
}
pub struct TargetConfigure {
    name: String,

    build_triple:  Option<MachineTriple>,
    host_triple:   Option<MachineTriple>,
    target_triple: Option<MachineTriple>,
}
impl TargetConfigure {
    pub fn configure(&self, build: &Build) -> Target {
        
    }
}

pub type TargetId = uint;
pub type SubtargetId = uint;

// Note Target is also a subtarget; specifically, it's the target's target
pub struct Target {
    id: TargetId,
    name: String,

    custom_subtargets: Vec<Subtarget>,

    build_subtarget: Option<Subtarget>,
    host_subtarget: Option<Subtarget>,

    triple: MachineTriple,
}
impl Target {
    pub fn build(&mut self,
                 build: &Build) {
        if self.crates.len() == 0 {
            
        } else {
            
        }
    }
}

pub struct Subtarget {
    id: SubtargetId,
    name: String,

    triple: MachineTriple,
}
impl Subtarget {
}

pub trait SubtargetEsk {
    fn address_prefix(&self) -> address::Prefix;

    fn triple<'a>(&'a self) -> &'a MachineTriple;

    fn build_default_config(&self) -> CrateConfig {
        
    }
}

pub struct Extern {
    src_path: Path,
    buildcrate: buildcrate::Crate,
}


pub struct MachineTriple(String);
impl fmt::Show for MachineTriple {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> fmt::Result {
        let &MachineTriple(ref inner) = self;
        inner.fmt(fmt)
    }
}

pub enum PreferenceLevel {
    UserPrefLevel,
    BuildPrefLevel,
    TargetPrefLevel,
    SubtargetPrefLevel,
}
pub enum LogDestination {
    
}

pub struct Preferences {
    level: PreferenceLevel,
    
    logging: Option<bool>,
    verbose: Option<bool>,
    
    trans_stats: Option<bool>,
   
}

// Takes an Origin in addition for diagnostics.
pub trait FromStrWithOrigin {
    fn from_str_with_origin(s: &str, origin: Origin) -> Option<Self>;
}
