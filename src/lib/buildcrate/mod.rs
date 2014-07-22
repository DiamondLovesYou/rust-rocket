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
use std::collections::TreeSet;
use std::comm::{Sender, Receiver};
use std::dynamic_lib::DynamicLibrary;

use syntax::crateid::CrateId;
use syntax::ast;

use rustc;

use uuid;

use address::Address;
use override::{Origin, DefaultOrigin, SpanOrigin};
use driver::{session, diagnostics};
use BUILD_CRATE_FILENAME;
use {SubtargetEsk, Build};

pub mod bootstrap;
pub mod build;
pub mod load;
pub mod rtio_wrapper;

pub type BuildCrateId = uuid::Uuid;

static PROJECT_SOURCE_PATH: &'static str = "src";

enum Message {
    
}
struct Session {
    recv: Receiver<Message>,
    
    source_crates: Vec<Crate>,
}
impl Session {
}
pub struct SessionIf {
    send: Sender<Message>,
}
impl SessionIf {
    
}

// Crate represents Things that must be known throughout a run about a specific crate.
// path && path_ts are stored in the DB, everything else is discovered 
pub struct Crate {
    // this path must be relative to the source dir
    path: Path,
    path_ts: u64,

    id: CrateId,
    deps: Vec<CrateId>,
}
pub struct BuildCrate {
    path: Path,
    dylib: Option<DynamicLibrary>,
}
impl BuildCrate {
    pub fn configure<TTarget: SubtargetEsk>(build: &Build,
                                            target: &TTarget) -> BuildCrate {

        let build_crate_path = build.src_path.join(BUILD_CRATE_FILENAME);
        if build_crate_path.exists() {
            // build the build crate as a dylib
            // TODO: overrides
            // TODO: build crate injections (for example, for use in Rust proper
            // in the bootstrapping binary).

        } else {
            diagnostics().fatal(DefaultOrigin,
                                "for the time begin, Rocket requires a build crate TODO");
        }
    }
}

/*pub fn find_crates_in_dir<TSess: Session>(_sess: &TSess, cwd: &Path) -> io::IoResult<~[Crate]> {
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
}*/

