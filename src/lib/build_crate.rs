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
use rustc;

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
pub struct BuildRs {
    ts: u64,
    // path to the built dylib:
    path: Path,
    dylib: DynamicLibrary,

}
pub struct BuildCrate {
    crates: Vec<(Crate, Origin)>,
    
    parent: Option<BuildCrate>,
}
impl BuildCrate {
    pub fn configure<TSess: session::Session>(sess: &TSess) -> BuildCrate {
        let build_crate_path = src_path.join(BUILD_CRATE_FILENAME);
        if build_crate_path.exists() {
            // build the build crate as a dylib
            // TODO: overrides
            
        } else {
            let crate_files = find_crate_ids_and_paths_in_dir(src_path)
                .move_iter()
                .map(|(_, p)| p )
                .collect();
            if crate_files.len() == 0 {
                // FIXME(diamond): report crate parse errors to file
                sess.fatal("no crates found")
            }

            BuildCrate {
                build_dylib: None,
                build_ts:    None,

            }
        }
    }
}

pub static EXPANSION_YARD_NAME: &'static str = "expansion";
pub static GATED_FEATURE_CHECKING_YARD_NAME: &'static str = "gated feature checking";

pub fn new_router_for_crate(sess: &session::Session_) -> Router {
    use rustc;
    use rustc::front;
    
    let parse_input_yard = Yard::new(~"parse input",
                                     DefaultOrigin,
                                     parse_input_hump,
                                     parse_input);
    fn parse_input_hump(push: HumpPush) {
        push(RustCfgKey, ScannedClass, DefaultOrigin);
        push(TypeIdCargoKey(type_id::<ast::Crate>()), LoadClass, DefaultOrigin);
    }
    fn parse_input(router: &session::Router, train: &mut Train) -> Diversion {

        use syntax::parse;

        let cfg = match train.by_override(RustCfgKey) {
            RustCfgValue(cfg) => cfg,
            _ => unreachable!(),
        };
        fn map_cfg(v: CfgMap) -> @ast::MetaItem {
            let (name, _, value) = v;
            let name_ident = token::intern_and_get_ident(name);
            let meta = match value {
                CfgValueNone => ast::MetaWord(name_ident),
                CfgValueInt(v) => ast::MetaNameValue(name_ident,
                                                     ast::LitInt(v, ast::TyI64)),
                CfgValueStr(string) => ast::MetaNameValue(name_ident, LitStr),
                CfgValueFloat(v) => {
                    ast::MetaNameValue(name_ident,
                                       LitFloat(token::intern_and_get_ident(v.to_str()),
                                                TyF64))
                }
                CfgValueBool(v) => ast::MetaNameValue(name_ident,
                                                      LitBool(v)),
                CfgValueMap(m) => ast::MetaList(name_ident,
                                                m.iter().map(map_cfg).collect()),
            };
            @dummy_spanned(meta)
        }
        let crate_cfg = cfg
            .iter()
            .map(map_cfg)
            .collect();
        let parse_sess = train.decouple();
        let parse_sess = @parse_sess;

        let input = train.main_input().expect("I need input!");
        let krate = parse::parse_crate_from_file(input, crate_cfg.clone(), parse_sess);
        train.couple(krate);

        let files = parse_sess
            .cm
            .files
            .borrow()
            .get()
            .iter()
            .map(|fm| Path::new(fm.name.clone()) )
            .collect();
        router.report_fs_deps(files);
        NoDiversion
    }

    /// Phase 2:

    fn gated_feature_checking_hump(push: HumpPush) {
        push(TypeIdCargoKey(type_id::<ast::Crate>()), ScannedClass, DefaultOrigin);
    }
    fn gated_feature_checking(router: &session::Router, train: &mut Train) -> Diversion {
        let sess = train.rustc_sess;
        front::feature_gate::check_crate(sess, train.by_type_id().get_ref());
        NoDiversion
    }
    fn refine_crate_hump(push: HumpPush) {
        push(TypeIdCargoKey(type_id::<ast::Crate>()), RefinedClass, DefaultOrigin);
    }
    fn crate_inject(router: &session::Router, train: &mut Train) -> Diversion {
        train.couple(front::std_inject(train.rustc_sess, train.decouple()));
        NoDiversion
    }
    fn strip_unconfigured_items_one(router: &session::Router, train: &mut Train) {
        train.couple(front::config::strip_unconfigured_items(train.decouple()));
        NoDiversion
    }
    fn find_syntax_phase_crates(router: &session::Router, train: &mut Train) {
        let mut visitor = SyntaxPhaseVisitor {
            router: router,
        };
        visit::walk_crate(&mut visitor, train.by_type_id().get_ref());
        NoDiversion
    }
    fn expansion(router: &session::Router, train: &mut Train) {
        let parse_sess = train.by_type_id();
        let parse_sess = @parse_sess;
        
        let mut loader = metadata::creader::Loader::new(train.rustc_sess);
        train.couple(syntax::ext::expand::expand_crate(parse_sess,
                                                       &mut loader,
                                                       train.decouple()));
        train.rustc_sess.cstore.reset();
        NoDiversion
    }
    fn strip_unconfigured_items_two(router: &session::Router, train: &mut Train) {
        train.couple(front::config::strip_unconfigured_items(train.decouple()));
        NoDiversion
    }
    fn prelude_inject(router: &session::Router, train: &mut Train) {
        train.couple(front::std_inject::maybe_inject_prelude(train.rustc_sess, train.decouple()));
        NoDiversion
    }
    fn assign_node_ids_and_map(router: &session::Router, train: &mut Train) {
        let (krate, map) = front::assign_node_ids_and_map(train.rustc_sess, train.decouple());
        train.couple(krate);
        train.couple(map);
        NoDiversion
    }

    /// Phase 3:

    fn find_link_phase_crates(train: &mut Train) {
        /// now that we finished expanding && striping configuration items,
        /// find the crates we depend on at link time and inform the right
        /// interface so it may begin building if necessary.
    }
}
enum ExternDepEntry {
    CrateDepEntry(ast::CrateNum, Span, Svh, CrateId),
    NativeDepEntry(~str),
}
struct SyntaxPhaseVisitor<'router> {
    router:  &'router session::Router,
}
impl<'router> syntax::visit::Visitor<()> for SyntaxPhaseVisitor<'router> {
    fn visit_view_item(&mut self, i: &syntax::ast::ViewItem, _: ()) {
        if i.attrs.iter().any(|attr| {
                attr.name().get() == "phase" &&
                    attr.meta_item_list().map_or(false, |phases| {
                        attr::contains_name(phases, "syntax")
                    })
            }) {
            match i.node {
                ast::ViewItemExternCrate(name, ref path_opt, _) => {
                    let name = token::get_ident(name);
                    let crate_id = path_opt
                        .and_then(|(path_str, _)| {
                            from_str(path_str.get())
                                .or_else(|| {
                                    self.router.err(SpanOrigin(i.span),
                                                    "malformed crate id");
                                    None
                                })
                        })
                        .or_else(|| from_str(name.get().to_str()))
                        .unwrap();
                    self.router.inject_crate_dep(EXPANSION_YARD_NAME.clone(),
                                                 Address::from_crate_id_with_phase(crate_id,
                                                                                   SyntaxPhase));
                }
                _ => {}
            }
        }
    }
}
