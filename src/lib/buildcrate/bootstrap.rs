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

use std::gc::{Gc, GC};
use std::intrinsics::TypeId;
use syntax;
use syntax::ast;
use syntax::codemap::{Span, DUMMY_SP, dummy_spanned, CodeMap, Spanned};
use syntax::parse::token::{InternedString, str_to_ident, intern,
                           intern_and_get_ident, get_ident};
use rustc;
use rustc::front;

use address::Address;
use driver::session;
use railroad::{TypeIdCargoKey, LoadClass, Yard, Train, Diversion,
               NoDiversion, ScannedClass, RefineryClass, CargoKey,
               Classification};
use railroad::{OverrideCargoKey, Yard, Router};
use override::{CfgMap, CfgKey};
use override::{SpanOrigin, DefaultOrigin, Origin, Originated, Od, originate};

pub fn override_cleanup_yard() -> Yard {

    fn hump(push: |CargoKey, Classification|) {
        
    }
    fn process(driver: &session::Router, train: &mut Train) -> Diversion {
        
    }

    Yard::new("override cleanup".to_string(),
              DefaultOrigin,
              process,
              hump)
}

pub static EXPANSION_YARD_NAME: &'static str = "expansion";
pub static GATED_FEATURE_CHECKING_YARD_NAME: &'static str = "gated feature checking";

pub fn new_router_for_crate(addr: Address) -> Router {
    use rustc::back::svh::Svh;
    let parse_input_yard = Yard::new("parse input".to_string(),
                                     DefaultOrigin,
                                     parse_input,
                                     parse_input_hump);
    fn parse_input_hump(push: |CargoKey, Classification|) {
        push(OverrideCargoKey(CfgKey), ScannedClass);
        push(TypeIdCargoKey(TypeId::of::<ast::Crate>()), LoadClass);
    }
    fn parse_input(router: &session::Router, train: &mut Train) -> Diversion {

        use syntax::parse;

        let cfg = train.by_override(CfgKey, None::<CfgMap>);
        fn map_cfg(v: CfgMap) -> Gc<ast::MetaItem> {
            use override::{CfgValueNone, CfgValueInt, CfgValueStr,
                           CfgValueFloat, CfgValueBool, CfgValueVec};
            let (name, _, value) = v;
            let name_ident = intern_and_get_ident(name);
            let meta = match value {
                CfgValueNone => ast::MetaWord(name_ident),
                CfgValueInt(v) => ast::MetaNameValue(name_ident,
                                                     dummy_spanned(ast::LitInt(v, ast::TyI64))),
                CfgValueStr(string) => ast::MetaNameValue(name_ident, ast::LitStr),
                CfgValueFloat(v) => {
                    ast::MetaNameValue(name_ident,
                                       ast::LitFloat(intern_and_get_ident(v.to_str()),
                                                     ast::TyF64))
                }
                CfgValueBool(v) => ast::MetaNameValue(name_ident,
                                                      ast::LitBool(v)),
                CfgValueVec(m) => ast::MetaList(name_ident,
                                                m.iter().map(map_cfg).collect()),
            };
            box(GC) dummy_spanned(meta)
        }

        let crate_cfg = (*cfg)
            .iter()
            .map(map_cfg)
            .collect();

        let parse_sess = train.decouple();
        let parse_sess = box(GC) parse_sess;

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

    fn gated_feature_checking_hump(push: |CargoKey, Classification|) {
        push(TypeIdCargoKey(TypeId::of::<ast::Crate>()), ScannedClass, DefaultOrigin);
    }
    fn gated_feature_checking(router: &session::Router, train: &mut Train) -> Diversion {
        let sess = train.rustc_sess;
        front::feature_gate::check_crate(sess, train.by_type_id().get_ref());
        NoDiversion
    }
    fn refine_crate_hump(push: |CargoKey, Classification|) {
        push(TypeIdCargoKey(TypeId::of::<ast::Crate>()), RefineryClass, DefaultOrigin);
    }
    fn std_inject(router: &session::Router, train: &mut Train) -> Diversion {
        use rustc::front::std_inject;
        train.couple(std_inject::maybe_inject_crates_ref(train.rustc_sess,
                                                         train.decouple()));
        NoDiversion
    }
    fn strip_unconfigured_items_one(router: &session::Router, train: &mut Train) {
        train.couple(front::config::strip_unconfigured_items(train.decouple()));
        NoDiversion
    }


    enum ExternDepEntry {
        CrateDepEntry(ast::CrateNum, Span, Svh, String),
        NativeDepEntry(String),
    }
    struct SyntaxPhaseVisitor<'router> {
        router:  &'router session::Router,
    }
    impl<'router> syntax::visit::Visitor<()> for SyntaxPhaseVisitor<'router> {
        fn visit_view_item(&mut self, i: &syntax::ast::ViewItem, _: ()) {
            use syntax::attr;

            use address::CratePluginPhase;

            if i.attrs.iter().any(|attr| {
                attr.name().get() == "phase" &&
                    attr.meta_item_list().map_or(false, |phases| {
                        attr::contains_name(phases, "syntax")
                    })
            }) {
                match i.node {
                    ast::ViewItemExternCrate(name, ref path_opt, _) => {
                        let name = get_ident(name);
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
                        let addr = Address::from_crate_id_with_phase(crate_id,
                                                                     CratePluginPhase);
                        self.router.inject_crate_dep(EXPANSION_YARD_NAME.clone(),
                                                     addr);
                    }
                    _ => {}
                }
            }
        }
    }

    fn find_syntax_phase_crates(router: &session::Router, train: &mut Train) {
        use syntax::visit;
        let mut visitor = SyntaxPhaseVisitor {
            router: router,
        };
        visit::walk_crate(&mut visitor, train.by_type_id());
        NoDiversion
    }
    fn expansion(router: &session::Router, train: &mut Train) {
        use rustc::plugin::load::Plugins;
        use rustc::plugin::registry::Registry;

        let parse_sess = train.by_type_id();
        let parse_sess = box(GC) parse_sess;

        // Windows dlls do not have rpaths, so they don't know how to find their
        // dependencies. It's up to us to tell the system where to find all the
        // dependent dlls. Note that this uses cfg!(windows) as opposed to
        // targ_cfg because syntax extensions are always loaded for the host
        // compiler, not for the target.
        if cfg!(windows) {
            train.rustc_sess.host_filesearch().add_dylib_search_paths();
        }
        let cfg = syntax::ext::expand::ExpansionConfig {
            deriving_hash_type_parameter: train.rustc_sess.features.default_type_params.get(),
            crate_id: train.by_type_id().clone(),
        };

        let &Plugins {
            ref macros,
            ref registry,
        } = train.by_type_id();
        let &Registry {
            ref syntax_exts,
            ref lint_passes,
            ..
        } = registry;

        {
            let mut ls = train.rustc_sess.lint_store.borrow_mut();
            for pass in lint_passes.move_iter() {
                ls.register_pass(Some(train.rustc_sess), true, pass);
            }
        }

        let krate: ast::Crate = train.decouple();
        let krate = syntax::ext::expand::expand_crate(&parse_sess,
                                                      cfg,
                                                      macros,
                                                      syntax_exts,
                                                      krate);
        train.couple(krate);
        NoDiversion
    }
    fn strip_unconfigured_items_two(router: &session::Router, train: &mut Train) {
        train.couple(front::config::strip_unconfigured_items(train.decouple()));
        NoDiversion
    }
    fn prelude_inject(router: &session::Router, train: &mut Train) {
        train.couple(front::std_inject::maybe_inject_prelude(train.rustc_sess,
                                                             train.decouple()));
        NoDiversion
    }
    fn assign_node_ids_and_map(router: &session::Router, train: &mut Train) {
        use rustc::front::assign_node_ids_and_map::assign_node_ids_and_map;
        let (krate, map) = assign_node_ids_and_map(train.rustc_sess, train.decouple());
        train.couple(krate);
        train.couple(map);
        NoDiversion
    }

    // Phase 3:

    fn find_link_phase_crates(train: &mut Train) {
        // now that we finished expanding && striping configuration items,
        // find the crates we depend on at link time and inform the right
        // interface so it may begin building if necessary.
    }
}
