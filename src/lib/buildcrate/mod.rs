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
use address::Address;
use std::comm::{Sender, Receiver};
use std::unstable::dynamic_lib::DynamicLibrary;
use syntax::crateid::CrateId;
use syntax::ast;
use collections::TreeSet;
use rustc;
use override::{Origin, DefaultOrigin, SpanOrigin};
use driver::session;
use super::BUILD_CRATE_FILENAME;
use super::{SubtargetEsk};

static PROJECT_SOURCE_PATH: &'static str = "src";
static VERSION_ATTR: &'static str = "rocket_version";
// the version we expect from a compiled build crate.
static COMPILED_VERSION: uint = 0;

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
    deps: TreeSet<CrateId>,
}
impl Crate {
    
}
pub struct LoadedBuildCrate {
    dylib: DynamicLibrary,

}
pub struct BuildCrate {
    // the newest timestamp from all files used in the build crate (including
    // the build crate itself).
    ts: u64,
    src_paths: Vec<Path>,
    // path to the built dylib:
    path: Path,

}
impl BuildCrate {
    pub fn configure<TSess: session::Session, TSub: SubtargetEsk>(sess: &TSess,
                                                                  src_path: &Path,
                                                                  rustb_path: &Path,
                                                                  sub: &TSub) -> BuildCrate {
        use rustc::driver::config::{FullDebugInfo, CrateTypeDylib};
        use rustc::driver::driver::FileInput;
        use rustc::driver::config::{basic_options, build_configuration};
        use rustc::driver::driver::{phase_1_parse_input};
        use rustc::driver::session::build_session;

        let build_crate_path = src_path.join(BUILD_CRATE_FILENAME);
        if build_crate_path.exists() {
            // build the build crate as a dylib
            // TODO: overrides
            // TODO: build crate injections (for example, for use in Rust proper
            // in the bootstrapping binary).

            let mut bk = BuildCrate {
                ts: 0,
                src_paths: Vec::new(),

                path: build_crate_path.clone(),

            };

            let mut rc_sess = build_session({
                let mut opts = basic_options();
                opts.crate_types = vec!(CrateTypeDylib);
                opts.debuginfo = FullDebugInfo;
                opts
            },
                                         Some(build_crate_path.clone()));
            let cfg = build_configuration(&sess);
            let input = FileInput(build_crate_path.clone());
            let krate = phase_1_parse_input(&sess, cfg, &input);

            // TODO: ask Cargo to resolve extern crates used by the build crate.

            check_version(sess, &krate);
        } else /* !build_crate_path.exists() */ {
        }
    }
}

pub fn check_version<TSess: session::Session>(sess: &TSess, krate: &ast::Crate) {
    use syntax::attr::AttrMetaMethods;
    let vers: Vec<ast::Attribute> = krate.attrs.iter()
        .filter(|attr| attr.check_name(VERSION_ATTR) )
        .collect();
    if vers.len() > 1 {
        for i in vers.move_iter() {
            sess.error(SpanOrigin(i.span),
                       "multiple Rocket version attributes provided");
        }
        sess.abort_if_errors();
    } else if vers.len() == 0 {
        sess.fatal(DefaultOrigin,
                   format!("missing \\#![rocket_version = \"{}\"]",
                           VERSION_ATTR));
    } else if vers[0].value_str().is_none() {
        sess.fatal(SpanOrigin(vers[0].span),
                   "incorrect rocket_version attribute");
    }
    let ver = vers[0].value_str().unwrap();
    if ver != VERSION_ATTR {
        sess.error(SpanOrigin(vers[0].span),
                   "Rocket is, at the moment, experimental. At present,
                    no backward compatibility is available.");
        sess.note(DefaultOrigin,
                  "Backward compatibility will begin at 1.0");
        sess.abort_if_errors();
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

