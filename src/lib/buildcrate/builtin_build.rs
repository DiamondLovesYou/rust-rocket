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

// This serves as the default build crate. It implements the basic overrides for
// all parts of the build process that Rocket will cover.
//
// Additionally, if the project lacks a build crate, Rocket will look for a
// lib.rs or a main.rs in all sub directories of src in the project root.

use std::gc::{Gc, GC};

use syntax;
use syntax::ast;
use syntax::codemap::{Span, DUMMY_SP, dummy_spanned};
use syntax::crateid::CrateId;
use syntax::ext;
use syntax::ext::base::ExtCtxt;
use syntax::fold;
use syntax::owned_slice::OwnedSlice;
use syntax::parse::token::{InternedString, str_to_ident, intern,
                           intern_and_get_ident, get_ident};
use rustc::back::svh::Svh;
use rustc::driver::session::Session;
use uuid::Uuid;

use SubtargetEsk;

use address::Address;
use driver::{diagnostics, session};
use override;
use override::{SpanOrigin, DefaultOrigin};
use railroad::{Yard, Router, HumpPush};

pub fn new_default() {
}

static TRAITS_VERSION: u64 = 1;
static GET_BUILD_TRAITS_SYMBOL_NAME: &'static str = "rocket-get_build_traits";
static NAME_PREFIX: &'static str = "rocket-";

// This structure is embedded into built buildcrates as a static.
// It contains all info needed by rocket for its decision tree.
// The fields are pub to allow us to construct it in the user's build crate.
pub struct BuildCrateTraits {
    // For new built build crates, version will correspond to TRAITS_VERSION.
    pub version: u64,

    // the newest timestamp from all files used in the build crate (including
    // the build crate itself).
    // the paths to the build crate src files.
    // For example, in Linux distribution build crates, this will be None.
    pub src_paths: Option<(u64, &'static [&'static str])>,

    pub root:  &'static str,

    pub child_build_crates: &'static [fn() -> &'static BuildCrateTraits],
}

struct BuildTraitsInjecter<'a> {
    child_crateids: Vec<CrateId>,
    child_crateids_injected: bool,

    src_paths: (u64, Vec<Path>),
    root: Path,


    sess: &'a Session,
    // this isn't used for any macro expanding, it's just here because it
    // provides a large number of helpful functions to generate ast.
    ext: ext::base::ExtCtxt<'a>,
}
impl<'a> BuildTraitsInjecter<'a> {
    pub fn new(sess: &'a Session, cid: CrateId,
               children: Vec<CrateId>,
               cfg: ast::CrateConfig, root: Path) {
        let ecfg = syntax::ext::expand::ExpansionConfig {
            deriving_hash_type_parameter: sess.features.default_type_params.get(),
            crate_id: cid,
        };

        let files = sess.codemap().files.borrow()
            .iter()
            .filter(|fmap| fmap.is_real_file() )
            .map(|fmap| Path::new(fmap.name.to_string()) )
            .collect();

        let ts = files
            .iter()
            .filter_map(|file| {
                use std::io::fs::stat;
                match stat(file) {
                    Err(_) => unimplemented!(),
                    Ok(stat) => stat.modified,
                }
            })
            .fold(0, |acc, ts| {
                use std::cmp::max;
                max(acc, ts)
            });

        BuildTraitsInjecter {
            child_crateids: Vec::new(),
            child_crateids_injected: false,

            src_paths: (ts, files),

            root: root,

            get_version_inserted: false,
            sess: sess,
            ext: ExtCtxt::new(&sess.parse_sess, cfg, ecfg),         
        }
    }
    fn build_crate_traits_path(&self) -> ast::Path {
        let rustb_seg = ast::PathSegment {
            identifier: str_to_ident("rocket"),
            lifetimes: Vec::new(),
            types: OwnedSlice::empty(),
        };
        let buildcrate_seg = ast::PathSegment {
            identifier: str_to_ident("buildcrate"),
            lifetimes: Vec::new(),
            types: OwnedSlice::empty(),
        };
        let builtin_seg = ast::PathSegment {
            identifier: str_to_ident("builtin_build"),
            lifetimes: Vec::new(),
            types: OwnedSlice::empty(),
        };
        let traits_seg = ast::PathSegment {
            identifier: str_to_ident("BuildCrateTraits"),
            lifetimes: Vec::new(),
            types: OwnedSlice::empty(),
        };
        self.ext
            .ty_path(Path {
                span: DUMMY_SP,
                global: true,
                segments: vec!(rustb_seg, buildcrate_seg, builtin_seg, traits_seg),
            },
                     None)
    }

    fn build_crate_traits_expr(&self) -> Gc<ast::Expr> {
        let path = self.build_crate_traits_path();

        let (ts, ref paths) = self.src_paths;
        let src_paths_expr =
            ast::ExprTup(vec!(
                self.ext.expr_lit(ast::LitUint(ts, ast::TyU64)),
                ast::ExprVstore(
                    paths.iter()
                        .map(|p| {
                            self.ext.expr_lit
                                (ast::LitStr
                                 (intern_and_get_ident
                                  (p.display().to_str().as_slice()),
                                  ast::CookedStr))
                        })
                        .collect(),
                    ast::ExprVstoreSlice)));

        let children_exprs = vec!(
            );

        let fields = vec!(
            ast::Field {
                ident: dummy_spanned(str_to_ident("version")),
                expr:  self.ext.expr_lit(ast::LitUint(TRAITS_VERSION, ast::TyU64)),
                span:  DUMMY_SP,
            },
            ast::Field {
                ident: dummy_spanned(str_to_ident("src_paths")),
                expr:  self.ext.expr_some(DUMMY_SP,
                                          box(GC) ast::Expr {
                                              id: self.sess.next_node_id(),
                                              node: src_paths_expr,
                                              span: DUMMY_SP,
                                          }),
                span:  DUMMY_SP,
            },
            ast::Field {
                ident: dummy_spanned(str_to_ident("root")),
                expr:  self.ext.expr_lit(ast::LitStr(
                    intern_and_get_ident(self.root.display().to_str().as_slice()),
                    ast::CookedStr)),
                span:  DUMMY_SP,
            },
            ast::Field {
                ident: dummy_spanned(str_to_ident("child_build_crates")),
                expr:  self.ext.expr_vec_slice(DUMMY_SP, children_exprs),
                span:  DUMMY_SP,
            });
        self.ext
            .expr_struct(DUMMY_SP,
                         path,
                         fields)
    }
}

impl<'a> fold::Folder for BuildTraitsInjecter<'a> {
    fn fold_mod(&mut self, module: &ast::Mod) -> ast::Mod {
        let filtered_items: Vec<&Gc<ast::Item>> = module.items.iter()
            .filter(|&a| self.filter_item(*a))
            .collect();

        let mut flattened_items_iter = filtered_items.move_iter()
            .flat_map(|&x| self.fold_item(x).move_iter());
        let mut flattened_items = if !self.child_crateids_injected {
            self.child_crateids_injected = true;
            let global_traits = self.ext
                .item_static(DUMMY_SP,
                             str_to_ident("ROCKET-TRAITS"),
                             self.build_crate_traits_path(),
                             ast::MutImmutable,
                             self.build_crate_traits_expr());

            // add extern crate view items for child crates:
            self.child_crateids
                .iter()
                .map(|c| str_to_ident(c.name.as_slice()) )
                .map(|ident| {
                    ast::ViewItemExternCrate(ident,
                                             Some((get_ident(ident),
                                                   ast::CookedStr)),
                                             self.sess.next_node_id())
                })
                .chain(flattened_items_iter)
                .collect()
        } else {
            flattened_items_iter.collect()
        };


    }
}

pub fn build<T: SubtargetEsk>(target: &SubtargetEsk, krate: &Path, out: &Path) {
    use rustc::driver::config::{build_configuration, basic_options,
                                FullDebugInfo, CrateTypeDylib};
    use rustc::driver::driver::{OutputFilenames, FileInput,
                                phase_1_parse_input,
                                phase_2_configure_and_expand};
    use rustc::driver::session::{build_session};
    use rustc::back::link;

    let mut rc_sess = build_session(
        {
            let mut opts = basic_options();
            opts.crate_types = vec!(CrateTypeDylib);
            opts.target_triple = target.triple().to_str();
            opts.debuginfo = FullDebugInfo;
            opts
        },
        Some(krate.clone()));
    let cfg = build_configuration(&rc_sess);
    let input = FileInput(krate.clone());
    let krate = phase_1_parse_input(&rc_sess, cfg, &input);

    // TODO: ask Cargo to resolve extern crates used by the build crate.
    let outputs = OutputFilenames {
        out_directory: out.dir_path(),
        out_filestem: "dylib",
        single_output_file: Some(out.clone()),
    };
    let id = link::find_crate_name(Some(&rc_sess),
                                   krate.attrs.as_slice(),
                                   &input);
    let (krate, ast_map) =
        phase_2_configure_and_expand(&rc_sess,
                                     krate,
                                     &id);
    let traits = BuildTraitsInjecter::new(rc_sess,
                                          id.clone(),
                                          cfg.clone(),
                                          Vec::new(),    // TODO: child crates
                                          );
    let krate = ast_map.fold_crate(krate);

    super::check_version(rc_sess, &krate);
}

fn check_static_item(ty: &ast::Ty, expr: &ast::Expr) {
    // match &'static str
    match ty {
        ast::Ty {
            node: ast::TyRptr(
                Some(ast::Lifetime {
                    name: lifetime, ..
                }),
                ast::MutTy {
                    ty: inner_ty,
                    mutbl: ast::MutImmutable,
                }),
            ..
        } if lifetime == intern("static") => {
            match inner_ty {
                ast::Ty {
                    node: ast::TyPath(ast::Path {
                        global: false,
                        segments: ref segs,
                    }, None, _)
                } if segs.len() == 1 &&
                    segs[0].identifier == str_to_ident("str") => {
                        
                    }
            }
        }
    }
}

impl Override {
    pub fn new(attr: AttributeMeta,
               item: &ast::Item) -> Override {
        if attr.addrs.is_none() {
            diagnostics().error(SpanOrigin(item.span),
                                "missing address attribute");
        }
        if attr.override.is_none() {
            diagnostics().error(SpanOrigin(item.span),
                                "missing override attribute");
        }

	Override {
            id: item.id,
            span: item.span,
            
            addrs: attr.addrs.unwrap(),
            override: attr.override.unwrap(),
        }
    }
}

// Information related to an override at the ast level. Note that no type info
// is present.
#[deriving(Clone, Encodable, Decodable, Hash, Eq)]
pub struct Override {
    // The node of the value. We use this later to resolve the fully mangled
    // name of the symbol.
    id:       ast::NodeId,
    addrs:    Vec<(Address, Span)>,
    span:     Span,
    override: override::Override,
}


struct OverrideCollectorMap {
    overrides: Vec<Override>,
}

struct AttributeMeta {
    addrs: Option<Vec<(String, Span)>>,
    level: Option<(uint, Span)>,
    multi_set_kind: Option<(String, Span)>,
    multi_set_position: Option<(String, Span)>,
    override: Option<(String, String, Span)>,
}

pub fn expect_word<'a>(meta: Gc<ast::MetaItem>) -> String {
    match meta.value {
        ast::MetaWord(ref word) => {
            word.get().clone()
        }
        _ => {
            diagnostics().fatal()
        }
    }
}

fn rocket_meta<'a>(value: &'a ast::MetaItem) -> Option<&'a Vec<Gc<ast::MetaItem>>> {
    match value.node {
        ast::MetaList(ref name, ref meta) if name == InternedString::new("rocket") =>
            Some(meta),
        _ => None,
    }
}
fn filter_rocket_meta(attr: &ast::Attribute) -> bool {
    !attr.is_sugared_doc && rocket_meta(attr.value).is_none()
}

impl OverrideCollectorMap {
    pub fn new() -> OverrideCollectorMap {
        OverrideCollectorMap {
            overrides: Vec::new(),
        }
    }
}
impl fold::Folder for OverrideCollectorMap {
    fn fold_mod(&mut self, module: &ast::Mod) -> ast::Mod {
        let filtered_items: Vec<&Gc<ast::Item>> = module.items.iter()
            .filter(|&a| self.filter_item(*a))
            .collect();

        let flattened_items = module.items
            .iter()
            .flat_map(|&x| self.fold_item(x).move_iter())
            .collect();

        let filtered_view_items = module.view_items
            .iter()
            .map(|a| self.fold_view_item(a) )
            .collect();

        ast::Mod {
            inner: module.inner,
            view_items: filtered_view_items,
            items: flattened_items
        }
    }

    fn fold_item(&mut self, item: &ast::Item) -> ast::Item {
        let metas = item.attrs
            .iter()
            .filter(|attr| !attr.is_sugared_doc )
            .filter_map(|attr| attr.value )
            .filter_map(rocket_meta)
            .flat_map(|metalist| metalist.iter());

        if metas.len() == 0 {
            return item;
        }

        let mut attrs = AttributeMeta {
            addrs: None,
            level: None,
            multi_set_kind: None,
            multi_set_position: None,
        };

        let addr_attr = InternedString::new("address");
        let level_attr = InternedString::new("level");
        let multi_set_kind_attr = InternedString::new("multi_set_op");
        let multi_set_pos_attr =
            InternedString::new("multi_set_pos");
        let override_attr = InternedString::new("override");
        for meta in metas.move_iter() {
            match meta.node {
                ast::MetaNameValue(ref name, ref value) if name == addr_attr => {
                    match attrs.addr {
                        Some(ref mut addrs) => {
                            addrs.push((value, meta.span));
                        }
                        None => {
                            attrs.addr = Some(vec!((value, meta.span)))
                        }
                    }
                }
                ast::MetaNameValue(ref name, ref value) if name == level_attr => {
                    if attrs.level.is_some() {
                        diagnostics().error(SpanOrigin(meta.span),
                                            "multiple level attributes \
                                            provided");
                        continue;
                    }
                    match from_str(value) {
                        Some(level) => {
                            attrs.level = Some((level, meta.span));
                        }
                        None => {
                            diagnostics().error(SpanOrigin(meta.span),
                                                format!("expected integer; \
                                                        found `{}`",
                                                        value).as_slice());
                        }
                    }
                }
                ast::MetaNameValue(ref name, ref value) if name == multi_set_kind_attr => {
                    if attrs.multi_set_kind.is_some() {
                        diagnostics().error(SpanOrigin(meta.span),
                                            "multiple multi_set_ops \
                                            attributes provided");
                        continue;
                    }
                    attrs.multi_set_kind = Some((value.clone(), meta.span));
                }
                ast::MetaNameValue(ref name, ref value) if name == multi_set_pos_attr => {
                    if attrs.multi_set_position.is_some() {
                        diagnostics().error(SpanOrigin(meta.span),
                                            "multiple multi_set_pos \
                                             attributes provided");
                        continue;
                    }
                    attrs.multi_set_position = Some((value.clone(),
                                                     meta.span));
                }
                ast::MetaNameValue(ref name, ref value)
                    if name == override_attr => {
                    if attrs.override.is_some() {
                        diagnostics().error(SpanOrigin(meta.span),
                                            "multiple overrides \
                                             not permitted");
                        continue;
                    }
                    attrs.override = Some((value.clone(),
                                           meta.span));
                }
                _ => {
                    diagnostics().error(SpanOrigin(meta.span),
                                        "unknown attribute");
                }
            }
        }

        let override = Override::new(attrs, item.id);
        self.override.push(override);

        // remove our attributes:
        ast::Item {
            attrs: item.attrs
                .iter()
                .filter(filter_rocket_meta)
                .collect(),
            .. item
        }
    }
}

pub static EXPANSION_YARD_NAME: &'static str = "expansion";
pub static GATED_FEATURE_CHECKING_YARD_NAME: &'static str = "gated feature checking";

pub fn new_router_for_crate(sess: &session::Session, addr: Address) -> Router {
    use std::intrinsics::TypeId;
    use rustc;
    use rustc::front;

    use railroad::{TypeIdCargoKey, LoadClass, Yard, Train, Diversion,
                   NoDiversion, ScannedClass, RefineryClass};
    use override::{CfgMap, RustCfgOverride};
    
    let parse_input_yard = Yard::new(&"parse input".to_strbuf(),
                                     DefaultOrigin,
                                     parse_input_hump,
                                     parse_input);
    fn parse_input_hump(push: HumpPush) {
        push(RustCfgOverride, ScannedClass, DefaultOrigin);
        push(TypeIdCargoKey(TypeId::of::<ast::Crate>()), LoadClass, DefaultOrigin);
    }
    fn parse_input(router: &session::Router, train: &mut Train) -> Diversion {

        use syntax::parse;

        let cfg = train.by_override(RustCfgOverride);
        fn map_cfg(v: CfgMap) -> Gc<ast::MetaItem> {
            use override::{CfgValueNone, CfgValueInt, CfgValueStr,
                           CfgValueFloat, CfgValueBool, CfgValueVec};
            let (name, _, value) = v;
            let name_ident = intern_and_get_ident(name);
            let meta = match value {
                CfgValueNone => ast::MetaWord(name_ident),
                CfgValueInt(v) => ast::MetaNameValue(name_ident,
                                                     ast::LitInt(v, ast::TyI64)),
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
        let crate_cfg = cfg
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

    fn gated_feature_checking_hump(push: HumpPush) {
        push(TypeIdCargoKey(TypeId::of::<ast::Crate>()), ScannedClass, DefaultOrigin);
    }
    fn gated_feature_checking(router: &session::Router, train: &mut Train) -> Diversion {
        let sess = train.rustc_sess;
        front::feature_gate::check_crate(sess, train.by_type_id().get_ref());
        NoDiversion
    }
    fn refine_crate_hump(push: HumpPush) {
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
enum ExternDepEntry {
    CrateDepEntry(ast::CrateNum, Span, Svh, CrateId),
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
