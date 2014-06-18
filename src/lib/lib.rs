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

#![crate_id = "rocket#0.1"]
#![comment = "A Rust project builder"]
#![license = "GNU/LGPLv3"]
#![crate_type = "dylib"]
#![crate_type = "rlib"]

#![feature(phase, managed_boxes, struct_variant, macro_rules)]

extern crate syntax;
extern crate rustc;
extern crate std;
extern crate glob;
extern crate term;
extern crate arena;
extern crate serialize;
#[phase(syntax, link)]
extern crate log;
extern crate green;
extern crate sqlite = "sqlite3#0.1";
extern crate bindgen;
extern crate collections;
extern crate rustuv;
extern crate sync;
extern crate time;
extern crate url;

use buildcrate::BuildCrate;

use syntax::ast::{CrateConfig, Name};
use syntax::diagnostic::{Emitter, Handler};
use std::path::Path;
use std::str::MaybeOwned;
use std::rc::{Rc, Weak};
use rustc::driver::config::CrateType;

pub mod buildcrate;
pub mod toolchain;
pub mod user;
pub mod driver;
pub mod platform_dep;
pub mod address;
pub mod override;
pub mod workcache;
pub mod railroad;
pub mod dep;

pub static BUILD_CRATE_FILENAME: &'static str = "build.rs";
pub static RUSTB_PATH: &'static str = ".rustb";
pub static MSTR_DB_FILENAME: &'static str = "master.db";
pub static DB_EXT: &'static str = "db";
pub static IN_TREE_BUILD_SUBPATH: &'static str = "build";
pub static TARGETS_PATH: &'static str = "target";

macro_rules! build_session {
    (
        enum Message {
            $($msg:ident ($($bind:pat),*): ($($ty:ty),*) => $block:block),*
        }
        struct Database {
            $version:expr,

            $($db_col:ident : $db_col_ty:ty),*
        }
        struct Session {
            $($sess_col:ident : $sess_col_ty:ty),*
        }
    ) => {
        enum Message {
            AddRefMsg,
            // if Some(..), send back the post de-reference ref count.
            // used during shutdown to assert there are no more refs.
            DropRefMsg(Option<Sender<u64>>),

            $($msg ($($ty),*)),*
        }
        // Stores all persistent data.
        struct Database {
            $($db_col : $db_col_ty),*
        }
        // Stores Stuff we don't want (or can't) save to disk.
        struct Session {
            db:   Database,

            ref_count: uint,

            diag: driver::DiagIf,

            
        }

        impl Session {
            fn new_task((port, chan): (Receiver<Message>,
                                       Sender<Message>),
                        diag: driver::DiagIf,
                        rustb: Path) -> Sender<Message> {
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

    }
}


struct SilentEmitter;

impl Emitter for SilentEmitter {
    fn emit(&self,
            _cmsp: Option<(&codemap::CodeMap, Span)>,
            _msg: &str,
            _lvl: Level) {}
    fn custom_emit(&self,
                   _cm: &codemap::CodeMap,
                   _sp: Span,
                   _msg: &str,
                   _lvl: Level) {}
}

pub fn mk_silent_handler() -> Handler {
    use syntax::diagnostic::mk_handler;
    mk_handler(box SilentEmitter)
}
pub fn mk_silent_span_handler(cm: @codemap::CodeMap) -> SpanHandler {
    SpanHandler {
        cm: cm,
        handler: mk_silent_handler(),
    }
}

static INITIAL_MSTR_DB: &'static [u8] = include_bin!("master.db");

fn create_initial_mstr_db(rustb_path: &Path) -> sqlite::Database {
    use std::io::{File, Open, ReadWrite};
    let mstr_db_f = rustb_path.join(MSTR_DB_FILENAME);
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
    addr: AddresserIf,

    targets: Vec<Target>,

    src_path: Path,
    build_path: Path,
    
    build_crate: Box<build_crate::Crate>,
    db: sqlite::Database,
    
    externs: Option<Vec<Extern>>,

    platform: platform::Session,
}

impl Build {
    pub fn new_in_tree(src_path: Path) -> Build {
        let rustb_path = src_path.join(RUSTB_PATH);
        let build_path = src_path.join(IN_TREE_BUILD_SUBPATH);
        
        
    }
    pub fn new_out_of_tree(config: Configure) -> Build {
        build.create_new_target("default".to_string());

        build
    }

    pub fn configure<TSess: session::Driver>(sess: &TSess,
                                             config: &session::Configure) -> Build {
        debug!("configuring `{}` in `{}`",
               config.src_path.display(),
               config.build_path.display());
        let build_crate_path = config.src_path.join(BUILD_CRATE_FILENAME);
        sess.ensure_dir(config.build_path);
        sess.ensure_dir(config.rustb_path);

        let db = create_initial_mstr_db(&config.rustb_path);
        assert!(db.exec(format!("INSERT INTO build (src_path) VALUES (\"{}\")",
                                config.src_path)).expect())

        let mut build = Build {
            src_path: config.src_path.clone(),
            build_path: config.build_path.clone(),
            rustb_path: config.rustb_path.clone(),
            
            build_crate: build_crate::new(sess, config.src_path),
            db: db,

            externs: None,
            
            targets: Vec::new(),
        };

        if config.create_default_target() {
            let target_sess = session::Target::new();
            build.create_target(target_sess);
        }
    }

    pub fn load<TSess: session::Driver>(sess: &TSess, path: &Path) -> Option<Build> {
        let db_path = path.join_many([RUSTB_PATH, RUSTB_DB_FILENAME]);
        
    }

    pub fn create_target(&mut self,
                         sess: &TSess,
                         tsess: session::Target) {
        let target = Target {
            name: sess.name,
            custom_subtargets: Vec::new(),
            build_subtarget: None,
            host_subtarget: None,
            mach: sess.mach,
            path: self.targets_path.join(sess.name),
        };

        sess.ensure_dir(self.targets_path);
        sess.ensure_dir(target.path);
        self.targets.push(target);
    }
    pub fn build(&mut self,
                 sess: &session::SessionIf,
                 target: Vec<String>) {
        for t in self.targets
            .mut_iter()
            .filter(|t| target.is_none() || t.name == target.unwrap() ) {
            t.build(sess, self);
        }
    }
}
pub trait Configure {
    /// configures a-fresh in a new build dir or reruns configure in an existing dir.
    fn configure(&mut self,
                 src_path: &Path,
                 build_path: &Path,
                 diag_handler: &diagnostic::Handler);
}
impl Configure for Option<Build> {
    fn configure(&mut self,
                 src_path: &Path,
                 build_path: &Path,
                 diag_handler: &diagnostic::Handler) {
        match self {
            Some(ref mut build) => {
                // re-configure:

                
            }
            None => {
                // fresh configure:

            }
        }
    }
}
pub type TargetId = uint;
pub type SubtargetId = uint;

// Note Target is also a subtarget; specifically, it's the target's target
pub struct Target {
    name: String,

    custom_subtargets: Vec<Subtarget>,

    build_subtarget: Option<Subtarget>,
    host_subtarget: Option<Subtarget>,

    triple: MachineTriple,
}
impl Target {
    pub fn build<TSess: session::Session>(&mut self,
                                          sess: &TSess,
                                          build: &Build) {
        if self.crates.len() == 0 {
            
        } else {
            
        }
    }
}

pub struct Subtarget {
    id: u64,
    name: Name,

    triple: MachineTriple,

    crates: Vec<Address>,
}
impl Subtarget {
}

pub trait SubtargetEsk {
    fn address_prefix(&self) -> address::Prefix;

    fn triple<'a>(&'a self) -> &'a MachineTriple;
}

pub struct Extern {
    crate_id: crateid::CrateId,
    dir: Path,
    build_crate: build_crate::Crate,
}

pub struct Crate {
    id: Option<CrateId>,
    file: Path,
}

pub trait ItemUse {
    // Is this resource available for use?
    fn is_available(&self) -> bool;

    // Is this resource locally managed? As in, are we resonsible for rebuilding
    // if stale? If not, we only concern ourselfs with its outputs.
    fn is_locally_managed(&self) -> bool;

    // Are this resource's rebuilds contained? ie do we need to rebuild all items
    // depending on this if this is rebuilt?
    fn are_rebuilds_contained(&self) -> bool;

    fn with_outputs<U>(&self, f: |&[Item]| -> U) -> U;
}

pub struct MachineTriple(String);
impl fmt::Show for MachineTriple {
    fn fmt(&self, fmt: &mut Formatter) -> fmt::Result {
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
pub fn early_error(msg: &str) -> ! {
    diagnostic::DefaultEmitter.emit(None, msg, diagnostic::Fatal);
    fail!(diagnostic::FatalError);
}
