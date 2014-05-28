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

#![crate_id = "rocket"]
#![comment = "The Rust project builder"]
#![license = "GNU/LGPLv3"]
#![crate_type = "bin"]
#![crate_type = "dylib"]
#![crate_type = "rlib"]

extern crate syntax;
extern crate rustc;
extern crate std;
extern crate getops;
extern crate glob;
extern crate term;
extern crate arena;
extern crate serialize;
#[phase(syntax, link)]
extern crate log;
extern crate green;
extern crate sqlite = "sqlite3#0.1";
extern crate bindgen;

pub mod build_crate;
pub mod cli;
pub mod toolchain_wrap;
pub mod user;
pub mod session;
pub mod platform_dep;

use build_crate::BuildCrate;

use syntax::ast::{CrateConfig, Name};
use syntax::diagnostic::{Emitter, Handler};
use std::path::Path;
use std::send_str::SendStr;
use std::rc::{Rc, Weak};
use rustc::driver::session::CrateType;

pub static BUILD_CRATE_FILENAME: &'static str = "build.rs";
pub static RUSTB_PATH: &'static str = ".rustb";
pub static MSTR_DB_FILENAME: &'static str = "master.db";
pub static DB_EXT: &'static str = "db";
pub static IN_TREE_BUILD_SUBPATH: &'static str = "build";
pub static TARGETS_PATH: &'static str = "target";

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
    Handler {
        err_count: Cell::new(0),
        emit: box SilentEmitter,
    }
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
        build.create_new_target("default".to_strbuf());

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
                 target: Option<~str>) {
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

/// Note Target is also a subtarget; specifically, it's the target target
pub struct Target {
    name: StrBuf,

    custom_subtargets: Vec<Subtarget>,

    build_subtarget: Option<Subtarget>,
    host_subtarget: Option<Subtarget>,

    mach: mach_target::Target,

    /// cache
    path: Path,
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

    triple: mach_target::Target,

    crates: ~[],
}
impl Subtarget {
    fn new() -> Subtarget {
        Subtarget{}
    }

    
}

pub struct Extern {
    crate_id: crateid::CrateId,
    dir: Path,
    build_crate: ~build_crate::Crate,
}

pub enum DebuggingKey {
    // TODO
}

pub type CodeGenOverrideValue   = OverrideValue<BooleanValue>;
pub type DebuggingOverrideValue = OverrideValue<BooleanValue>;
#[deriving(Encodable, Decodable)]
pub enum OverrideType {
    ArgumentOverride(ArgumentOverrideValue),
    CrateOverride(CrateOverrideValue),
}

pub trait Addressable {
    fn name(&self) -> Name;
    fn address(&self) -> Address;
}
pub struct Crate {
    id: Option<CrateId>,
    file: Path,    
}

pub trait ItemUse {
    // Is this resource available for use?
    pub fn is_available(&self) -> bool;

    // Is this resource locally managed? As in, are we resonsible for rebuilding
    // if stale? If not, we only concern ourselfs with its outputs.
    pub fn is_locally_managed(&self) -> bool;

    // Are this resource's rebuilds contained? ie do we need to rebuild all items
    // depending on this if this is rebuilt?
    pub fn are_rebuilds_contained(&self) -> bool;

    pub fn with_outputs<U>(&self, f: |&[Item]| -> U) -> U;
}

pub enum Branches {
    IfThen(&'static Item),
    IfThenElse(&'static Item,  // then
               &'static Item), // else
}
pub enum Source_ {
    FromSystem(&'static Item),
    FromDependancy(&'static Item),
}

pub enum Item_ {
    ToolItem(&'static Source),
    StaticItem(&'static str),
    BranchedItem(br: &'static Branches, cond: &'static Item),
}

pub struct GraphedNode<T> {
    id: u64,
    name: SendStr,
    span: syntax::Span,

    node: T,

    children: uint,
    parents:  uint,
}
pub struct StaleNode<T> {
    
}

pub enum Locus_ {
    /// Finds all crates in the immediate sub-directory.
    DirectoryLocus(SendStr),
    /// Specify 
    ExplicitLocus(SendStr),
}
pub type Locus = GraphedNode<Locus_>;


pub type CfgValue = GraphedNode<CfgValue_>;

pub struct Crate_ {
    locus: CrateLocus,

    cfg: HashMap<SendStr, CfgValue>,
}
pub type Crate = GraphedNode<Crate_>;

pub struct MachineTarget(~str);


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
