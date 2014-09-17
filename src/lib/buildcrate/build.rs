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

use std::collections::{HashMap, HashSet};
use std::default::Default;
use std::from_str::FromStr;
use std::fmt::Show;
use std::gc::{Gc, GC};
use std::path::GenericPath;

use syntax;
use syntax::ast;
use syntax::codemap::{Span, DUMMY_SP, dummy_spanned, CodeMap, Spanned};
use syntax::crateid::CrateId;
use syntax::ext;
use syntax::ext::base::ExtCtxt;
use syntax::ext::build::AstBuilder;
use syntax::fold::Folder;
use syntax::owned_slice::OwnedSlice;
use syntax::parse::token::{InternedString, str_to_ident, intern,
                           intern_and_get_ident, get_ident};
use syntax::util::small_vector::SmallVector;
use rustc::back::svh::Svh;
use rustc::driver::session::Session;
use uuid::Uuid;

use SubtargetEsk;
use FromStrWithOrigin;

use address::Address;
use driver::{diagnostics, session};
use driver::diagnostics::Driver;
use override;
use override::{SpanOrigin, DefaultOrigin, Origin, Originated, Od, originate};
use override::{MultiSetPosition, MultiSetOp};
use super::bootstrap::EXPANSION_YARD_NAME;
use railroad::{Yard, Router};

pub fn new_default() {
    unimplemented!()
}

pub type TraitsVersion = u64;
pub type GetTraitsFn = fn() -> (TraitsVersion, &'static ());

static TRAITS_VERSION: TraitsVersion = 1;
static GET_BUILD_TRAITS_SYMBOL_NAME: &'static str = "rocket-get_build_traits";
static NAME_PREFIX: &'static str = "rocket-";

// This structure is embedded into built buildcrates as a static.
// It contains all info needed by rocket for its decision tree.
// The fields are pub to allow us to construct it in the user's build crate.
pub struct BuildCrateTraits {
    // For new built build crates, version will correspond to TRAITS_VERSION.
    pub version: TraitsVersion,

    // the newest timestamp from all files used in the build crate (including
    // the build crate itself).
    // the paths to the build crate src files.
    // Linux distribution build crates will have a None here. This is because
    // those build crates will pre-built, and will be selected based on the
    // target. Thus Ubuntu distros can include build crates via packages and
    // provide seemless cross distro compilation for Arch Linux, etc.
    pub src_paths: Option<(u64, &'static [&'static str])>,

    pub root:  &'static str,

    pub child_build_crates: &'static [GetTraitsFn],
}

struct BuildTraitsInjecter<'a> {
    child_crateids: HashMap<Uuid, String>,
    child_crateids_injected: bool,

    src_paths: (u64, Vec<Path>),
    root: Path,


    sess: &'a Session,
    // this isn't used for any macro expanding, it's just here because it
    // provides a large number of helpful functions to generate ast.
    ext: ext::base::ExtCtxt<'a>,
}
impl<'a> BuildTraitsInjecter<'a> {
    pub fn new(sess: &'a Session, cid: String,
               children: HashMap<Uuid, String>,
               cfg: ast::CrateConfig, root: Path) -> BuildTraitsInjecter<'a> {
        let ecfg = syntax::ext::expand::ExpansionConfig {
            deriving_hash_type_parameter: sess.features.default_type_params.get(),
            crate_name: cid,
        };

        let files: Vec<Path> = sess.codemap().files.borrow()
            .iter()
            .filter(|fmap| fmap.is_real_file() )
            .map(|fmap| Path::new(fmap.name.to_string()) )
            .collect();

        let ts = files
            .iter()
            .map(|file| {
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
            child_crateids: children,
            child_crateids_injected: false,

            src_paths: (ts, files),

            root: root,

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
        ast::Path {
            span: DUMMY_SP,
            global: true,
            segments: vec!(rustb_seg, buildcrate_seg, builtin_seg, traits_seg),
        }
    }

    fn build_crate_traits_expr(&self) -> Gc<ast::Expr> {
        let path = self.build_crate_traits_path();

        let (ts, ref paths) = self.src_paths;
        let src_paths_expr =
            ast::ExprTup(vec!(
                self.ext.expr_lit(DUMMY_SP,
                                  ast::LitInt(ts, ast::UnsignedIntLit(ast::TyU64))),
                self.ext.expr_vec_slice(DUMMY_SP,
                                        paths.iter()
                                        .map(|p| {
                                            self.ext.expr_lit
                                                (DUMMY_SP, ast::LitStr
                                                 (intern_and_get_ident
                                                  (p.display().to_string().as_slice()),
                                                  ast::CookedStr))
                                        })
                                        .collect())));

        let children_exprs = vec!(
            );

        let fields = vec!(
            ast::Field {
                ident: dummy_spanned(str_to_ident("version")),
                expr:  self.ext.expr_lit(DUMMY_SP,
                                         ast::LitInt(TRAITS_VERSION,
                                                     ast::UnsignedIntLit(ast::TyU64))),
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
                expr:  self.ext.expr_lit(DUMMY_SP, ast::LitStr(
                    intern_and_get_ident(self.root.display().to_string().as_slice()),
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

impl<'a> Folder for BuildTraitsInjecter<'a> {
    fn fold_mod(&mut self, module: &ast::Mod) -> ast::Mod {
        let filtered_items: Vec<Gc<ast::Item>> = module.items
            .iter()
            .flat_map(|&a| self.fold_item(a).move_iter())
            .collect();

        let mut flattened_items_iter = filtered_items.move_iter()
            .flat_map(|x| self.fold_item(x).move_iter());

        let build_crate_traits_ty = self.ext.ty_path(self.build_crate_traits_path(),
                                                     None);

        let view_items = module.view_items
                .iter()
                .map(|i| self.fold_view_item(i) );

        if !self.child_crateids_injected {
            self.child_crateids_injected = true;
            let global_traits = self.ext
                .item_static(DUMMY_SP,
                             str_to_ident("ROCKET-TRAITS"),
                             build_crate_traits_ty,
                             ast::MutImmutable,
                             self.build_crate_traits_expr());

            // add extern crate view items for child crates:
            let view_items = self.child_crateids
                .iter()
                .map(|(c, b)| (str_to_ident(c.to_simple_str().as_slice()),
                               str_to_ident(b.as_slice())) )
                .map(|(uuid, ident)| {
                    ast::ViewItemExternCrate(ident,
                                             Some((get_ident(uuid),
                                                   ast::CookedStr)),
                                             self.sess.next_node_id())
                })
                .map(|view_| {
                    ast::ViewItem {
                        node: view_,
                        attrs: Vec::new(),
                        vis: ast::Inherited,
                        span: DUMMY_SP,
                    }
                })
                .chain(view_items)
                .collect();

            ast::Mod {
                inner: module.inner,
                view_items: view_items,
                items: Some(global_traits)
                    .move_iter()
                    .chain(flattened_items_iter)
                    .collect(),
            }
        } else {
            ast::Mod {
                inner: module.inner,
                view_items: view_items.collect(),
                items: flattened_items_iter.collect(),
            }
        };


    }
}

pub fn build<T: SubtargetEsk>(target: &T,
                              krate_path: &Path,
                              out: &Path) {
    use rustc::driver::config::{build_configuration, basic_options,
                                FullDebugInfo, CrateTypeDylib};
    use rustc::driver::driver::{OutputFilenames, FileInput,
                                phase_1_parse_input,
                                phase_2_configure_and_expand};
    use rustc::driver::session::{build_session_};
    use rustc::back::link;

    let mut rc_sess = {
        let handler = syntax::diagnostic::default_handler
            (syntax::diagnostic::Auto, None);

        build_session_(
            {
                let mut opts = basic_options();
                opts.crate_types = vec!(CrateTypeDylib);
                opts.target_triple = target.triple().to_string();
                opts.debuginfo = FullDebugInfo;
                opts
            },
            Some(krate_path.clone()),
            // TODO: route to our own internal diagnostic pluming.
            syntax::diagnostic::mk_span_handler(handler, CodeMap::new()))
    };
    let cfg = build_configuration(&rc_sess);
    let input = FileInput(krate_path.clone());
    let krate = phase_1_parse_input(&rc_sess, cfg, &input);

    // TODO: ask Cargo to resolve extern crates used by the build crate.
    let outputs = OutputFilenames {
        out_directory: out.dir_path(),
        out_filestem: "dylib".to_string(),
        single_output_file: Some(out.clone()),
        extra: "".to_string(),
    };
    let id = link::find_crate_name(Some(&rc_sess),
                                   krate.attrs.as_slice(),
                                   &input);
    let (krate, ast_map) =
        phase_2_configure_and_expand(&rc_sess,
                                     krate,
                                     id.as_slice(),
                                     None)
        .unwrap();

    check_version(&krate);

    let traits = BuildTraitsInjecter::new(&rc_sess,
                                          id.clone(),
                                          HashMap::new(),// TODO
                                          Vec::new(),    // TODO: child crates
                                          krate_path.dir_path());
    let krate = traits.fold_crate(krate);
}

fn check_static_item(ty: &ast::Ty, expr: &ast::Expr) {
    // match &'static str
    match ty {
        &ast::Ty {
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
            match *inner_ty {
                ast::Ty {
                    node: ast::TyPath(ast::Path {
                        global: false,
                        segments: ref segs,
                        ..
                    }, None, _),
                    ..
                } if segs.len() == 1 &&
                    segs.as_slice()[0].identifier == str_to_ident("str") => {

                    }
            }
        }
    }
}

// Information related to an override at the ast level. Note that no type info
// is present.
#[deriving(Clone, Encodable, Decodable)]
pub struct Override {
    // The node of the value. We use this later to resolve the fully mangled
    // name of the symbol.
    id:       ast::NodeId,
    addrs:    Vec<Originated<Address>>,
    span:     Span,
    level:    u64,
    overrides: Vec<Originated<override::Override>>,
}

impl Override {
    pub fn new(mut attr: AttributeMeta,
               item: &ast::Item) -> Override {
        if attr.addrs.is_none() {
            diagnostics().err(SpanOrigin(item.span),
                              "missing address attribute");
        }
        if attr.overrides.is_none() {
            diagnostics().err(SpanOrigin(item.span),
                              "missing override attribute");
        }

        diagnostics().fail_if_errors();

	Override {
            id: item.id,
            span: item.span,

            level: attr.level
                .map(|Originated { node: lvl, .. }| lvl )
                .unwrap_or_default(),
            addrs: attr.addrs.unwrap(),
                /*.move_iter()
                .map(|o| {
                    o.map(|addr, &origin| {
                        let addr = FromStr::from_str(addr.as_slice());
                        match addr {
                            Some(addr) => addr,
                            None => {
                                diagnostics().fatal(origin,
                                                    "invalid/unrecognized \
                                                     address");
                            }
                        }
                    })
                })
                .collect(),*/
            overrides: attr.overrides.unwrap(),
        }
    }
}

struct OverrideCollectorMap {
    overrides: Vec<Override>,
}

struct AttributeMeta {
    addrs: Option<Vec<Originated<Address>>>,
    level: Option<Originated<u64>>,
    overrides: Option<Vec<Originated<override::Override>>>,
}

fn rocket_meta<'a>(value: &'a ast::MetaItem) -> Option<&'a Vec<Gc<ast::MetaItem>>> {
    match value.node {
        ast::MetaList(ref name, ref meta)
            if *name == InternedString::new("rocket") => Some(meta),
        _ => None,
    }
}
fn filter_rocket_meta(attr: &ast::Attribute_) -> bool {
    !attr.is_sugared_doc && rocket_meta(&*attr.value).is_none()
}

impl OverrideCollectorMap {
    pub fn new() -> OverrideCollectorMap {
        OverrideCollectorMap {
            overrides: Vec::new(),
        }
    }
}
impl Folder for OverrideCollectorMap {
    fn fold_mod(&mut self, module: &ast::Mod) -> ast::Mod {
        let flattened_items = module.items
            .iter()
            .flat_map(|&x| self.fold_item(x).move_iter() )
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

    fn fold_item(&mut self, item: Gc<ast::Item>) -> SmallVector<Gc<ast::Item>> {
        use syntax::fold;

        let mut meta_lists = item.attrs
            .iter()
            .map(|attr| attr.node )
            .filter(|attr| !attr.is_sugared_doc )
            .flat_map(|attr| rocket_meta(&*attr.value).iter() );

        let mut attrs = AttributeMeta {
            addrs: None,
            level: None,
            overrides: None,
        };

        let addr_attr = InternedString::new("address");
        let level_attr = InternedString::new("level");
        let override_attr = InternedString::new("override");
        for metas in meta_lists {
            for meta in metas.iter() {
                let origin = SpanOrigin(meta.span);
                match meta.node {
                    ast::MetaNameValue(ref name, ref value) if *name == addr_attr => {
                        let addr = match value.node {
                            ast::LitStr(ref v, _) => {
                                FromStrWithOrigin::from_str_with_origin
                                    (v.get(), origin)
                            }
                            _ => {
                                expected_lit_str(meta.span);
                                continue;
                            }
                        };
                        let addr = match addr {
                            Some(addr) => addr,
                            None => continue,
                        };
                        attrs.addrs = attrs.addrs
                            .or_else(|| Some(Vec::new()) )
                            .map(|addrs| addrs.append_one(addr) )
                    }
                    ast::MetaNameValue(ref name, ref value) if *name == level_attr => {
                        if attrs.level.is_some() {
                            diagnostics().err(origin,
                                              "multiple level attributes \
                                               provided");
                            continue;
                        }
                        let value_origin = SpanOrigin(value.span);
                        attrs.level = match value.node {
                            ast::LitInt(c, _) if (c as i64) < 0 => {
                                diagnostics().err(value_origin,
                                                  "negative integers aren't allowed");
                                continue;
                            }
                            ast::LitInt(c, _)  =>
                                Some(originate(c as u64, value_origin)),
                            _ => {
                                diagnostics().err(value_origin,
                                                  "unexpected non-integer literal");
                                continue;
                            }
                        };
                    }
                    ast::MetaList(ref name, ref values) if *name == override_attr => {
                        attrs.overrides = attrs.overrides
                            .take()
                            .or_else(|| Some(Vec::new()) )
                            .map(|mut overrides| {
                                match check_override(values.as_slice(), origin) {
                                    Some(override) =>
                                        overrides.append_one(originate(override,
                                                                       origin)),
                                    None => overrides,
                                }
                            });
                    }
                    _ => {
                        diagnostics().err(origin, "unknown attribute");
                    }
                }
            }
        }

        let override = Override::new(attrs, &*item);
        self.overrides.push(override);

        // remove our attributes:
        let new_item = ast::Item {
            attrs: item.attrs
                .iter()
                .filter(|attr| filter_rocket_meta(&attr.node) )
                .map(|attr| attr.clone() )
                .collect(),
            .. *item
        };
        fold::noop_fold_item(&new_item, self)
    }
}

fn expected_lit_str(sp: Span) {
    diagnostics().err(SpanOrigin(sp),
                      "expected string literal");
}

fn check_override(values: &[Gc<ast::MetaItem>],
                  origin: Origin) -> Option<override::Override> {
    use override::{TOOL_ARG_KEY, TOOL_KEY, CRATE_KEY, CFG_KEY, CODEGEN_KEY};
    use override::{MULTI_SET_OP_ATTR, MULTI_SET_POS_ATTR};

    use platform_dep::ToolId;

    use FromStrWithOrigin;

    let tool_arg_key = InternedString::new(TOOL_ARG_KEY);
    let tool_key     = InternedString::new(TOOL_KEY);
    let crate_key    = InternedString::new(CRATE_KEY);
    let cfg_key      = InternedString::new(CFG_KEY);
    let codegen_key  = InternedString::new(CODEGEN_KEY);

    let multi_set_op_attr  = InternedString::new(MULTI_SET_OP_ATTR);
    let multi_set_pos_attr = InternedString::new(MULTI_SET_POS_ATTR);

    let mut multi_set_op: Option<OriginatedMultiSetOp> = None;
    let mut multi_set_pos = None;

    enum OverrideKey {
        Arg(ToolId),
        Tool(ToolId),
        Crate(Address),
        RustCfg,
        Codegen,
    }

    let mut key = None;

    impl FromStrWithOrigin for () {
        fn from_str_with_origin(_s: &str, _origin: Origin) -> Option<()> {
            Some(())
        }
    }

    fn set_key<T: FromStrWithOrigin>(key: &mut Option<Originated<OverrideKey>>,
                                     meta: Option<&ast::Lit_>,
                                     sp: Span,
                                     f: |T| -> OverrideKey) {
        match key {
            &Some(Originated { origin: origin, .. }) => {
                diagnostics().err(SpanOrigin(sp),
                                  "override type already provided");
                diagnostics().info(origin,
                                   "previous override type here");
            }
            _ => (),
        }
        let v: Option<Originated<T>> = match meta {
            Some(&ast::LitStr(ref value, _)) => {
                FromStrWithOrigin::from_str_with_origin(value.get(),
                                                        SpanOrigin(sp))
            }
            Some(_) => {
                expected_lit_str(sp);
                return;
            }

            None => {
                FromStrWithOrigin::from_str_with_origin("", SpanOrigin(sp))
            }
        };

        *key = v.map(|o| o.map(|v, _| f(v) ) );
    }

    for v in values.iter() {
        match v.node {
            ast::MetaNameValue(ref name, ref value) if *name == tool_arg_key => {
                set_key(&mut key,
                        Some(&value.node),
                        value.span,
                        |v| Arg(v) );
            }
            ast::MetaNameValue(ref name, ref value) if *name == tool_key => {
                set_key(&mut key,
                        Some(&value.node),
                        value.span,
                        |v| Tool(v) );
            }
            ast::MetaNameValue(ref name, ref value) if *name == crate_key => {
                set_key(&mut key,
                        Some(&value.node),
                        value.span,
                        |v| Crate(v) );
            }
            ast::MetaWord(ref name) if *name == cfg_key => {
                set_key(&mut key,
                        None,
                        v.span,
                        |()| RustCfg );
            }
            ast::MetaWord(ref name) if *name == codegen_key => {
                set_key(&mut key,
                        None,
                        v.span,
                        |()| Codegen );
            }

            ast::MetaNameValue(ref name, _) |
            ast::MetaList(ref name, _) if *name == multi_set_op_attr => {
                check_multi_set_op(&mut multi_set_op, &v.node, v.span);
            }

            ast::MetaNameValue(ref name, _) |
            ast::MetaList(ref name, _) if *name == multi_set_pos_attr => {
                check_multi_set_pos(&mut multi_set_pos, &v.node, v.span);
            }
        }
    }

    let ret = match key {
        None => {
            diagnostics().err(origin, "missing override type");
            None
        }

        Some(Originated { node: Tool(tool), .. }) =>
            Some(override::ToolOverride(tool)),
        Some(Originated { node: Arg(tool), .. }) => {
            let op: MultiSetOp<String> = match multi_set_op
                .take()
                .map(|Originated { node: node, .. }| node ) {
                    Some(op) => op,
                    None => Default::default(),
                };

            let pos = multi_set_pos
                .take()
                .map(|Originated { node: node, .. }| node );

            Some(override::ArgOverride(tool,
                                       override::MultiSet {
                                           op: op,
                                           pos: pos,
                                       }))
        }
        Some(Originated { node: Crate(addr), .. }) => {
            let op = multi_set_op
                .take()
                .map(|Originated { node: node, .. }| {
                    match node {
                        override::AddMSK => override::AddSK,
                        override::RemoveMSK => override::RemoveSK,
                        override::ReplaceMSK {
                            what: what,
                            ..
                        } => override::ReplaceSK(what),
                    }
                })
                .unwrap_or_default();
            Some(override::CrateOverride(addr, op))
        }
        Some(Originated { node: Cfg, .. }) => Some(override::RustCfgOverride),
        Some(Originated { node: Codegen, .. }) =>
            Some(override::CodegenOverride),
    };

    match multi_set_pos {
        Some(origin) => {
            origin.warn_unused("unused multi_set_pos attribute");
        }
        None => (),
    }
    match multi_set_op {
        Some(origin) => {
            origin.warn_unused("unused multi_set_op attribute");
        }
        None => (),
    }
    ret
}

fn check_is_none<T, M: Show>(attr: &Option<Originated<T>>,
                             span: Span,
                             msg:  M) -> bool {
    match attr {
        &Some(Originated { origin: origin, .. }) => {
            diagnostics().err(SpanOrigin(span),
                              format!("multiple {} attributes provided", msg));
            diagnostics().info(origin,
                               format!("location of previous {} attribute", msg));
            true
        }
        &None => false,
    }
}
type OriginatedMultiSetPos = Originated<MultiSetPosition<String>>;
type OriginatedMultiSetOp  = Originated<MultiSetOp<String>>;
fn check_multi_set_pos(attr: &mut Option<OriginatedMultiSetPos>,
                       item: &ast::MetaItem_,
                       span: Span) {
    fn check_pos_is_none(attr: &Option<OriginatedMultiSetPos>,
                         span: Span) -> bool {
        check_is_none(attr, span, "multi_set_pos")
    }
    if check_pos_is_none(attr, span) { return; }

    let before_str = InternedString::new("before");
    let after_str  = InternedString::new("after");
}

fn check_multi_set_op(attr: &mut Option<OriginatedMultiSetOp>,
                      item: &ast::MetaItem_,
                      span: Span) {
    fn check_op_is_none(attr: &Option<OriginatedMultiSetOp>,
                        span: Span) -> bool {
        check_is_none(attr, span, "multi_set_op")
    }
    if check_op_is_none(attr, span) { return; }

    let add_op = InternedString::new("add");
    let remove_op = InternedString::new("remove");
    let replace_op = InternedString::new("replace");

    let origin = SpanOrigin(span);

    let check_op = |value: &InternedString| -> Option<OriginatedMultiSetOp> {
        if *value == add_op {
            Some(originate(override::AddMSK, origin))
        } else if *value == remove_op {
            Some(originate(override::RemoveMSK, origin))
        } else {
            let msg = if *value == replace_op {
                "`replace` requires an attribute list"
            } else {
                "unknown multi-set operation; expected `add`, `remove`, or `replace`"
            };
            diagnostics().err(origin, msg);
            None
        }
    };

    match item {
        &ast::MetaNameValue(_,
                            Spanned {
                                node: ast::LitStr(ref value, _),
                                ..
                            }) => {
            // a single `add` or `replace`.
            *attr = check_op(value);
        }
        &ast::MetaNameValue(_,
                            Spanned {
                                span: value_span,
                                ..
                            }) => {
            diagnostics().err(SpanOrigin(value_span),
                              "expected string literal");
            return;
        }
        &ast::MetaList(_, ref list) => {
            let what_str  = InternedString::new("what");
            let count_str = InternedString::new("count");

            let mut what: Option<HashSet<Od<override::TagOrValueKey<String>>>>
                = None;
            let mut count: Option<Od<u64>> = None;
            for m in list.iter() {
                match m.node {
                    ast::MetaWord(ref name) if *name != replace_op => {
                        if check_op_is_none(attr, m.span) { return; }
                        *attr = check_op(name);
                    }
                    ast::MetaWord(ref name) if *name == replace_op => {
                        if check_op_is_none(attr, span) { return; }
                        *attr = Some(originate(override::ReplaceMSK {
                            // these are dummy values. We set these after we finish the loop.
                            what: HashSet::new(),
                            count: None,
                        },
                                                  origin));
                    }
                    ast::MetaNameValue(ref name,
                                       Spanned {
                                           node: value,
                                           span: value_span,
                                       }) if *name == count_str => {
                        match count {
                            Some(c) => {
                                diagnostics().err(SpanOrigin(value_span),
                                                  "count already provided");
                                diagnostics().info(c.origin,
                                                   "previous count here");
                                return;
                            }
                            None => {}
                        }
                        count = match value {
                            ast::LitInt(0, _) => {
                                diagnostics().err(SpanOrigin(value_span),
                                                  "unexpected zero count");
                                return;
                            }
                            ast::LitInt(c, _) if (c as i64) < 0 => {
                                diagnostics().err(SpanOrigin(value_span),
                                                  "negative integers aren't allowed");
                                return;
                            }
                            ast::LitInt(c, _) =>
                               Some(originate(c, SpanOrigin(value_span))),
                            _ => {
                                diagnostics().err(SpanOrigin(value_span),
                                                  "unexpected non-integer literal");
                                return;
                            }
                        };
                    }
                    ast::MetaList(ref name,
                                  ref value) if *name == what_str => {
                        let tagged_str = InternedString::new("tag");
                        let value_str  = InternedString::new("value");

                        fn expected_single_kvp(sp: Span) {
                            diagnostics().err(SpanOrigin(sp),
                                              "expected a single key-value pair");
                        }

                        if value.len() != 1 {
                            expected_single_kvp(m.span);
                            return;
                        }
                        let new_what = match value[0].node {
                            ast::MetaNameValue(ref name,
                                               Spanned {
                                                   node: ast::LitStr(ref tag, _),
                                                   span: sp,
                                               }) if *name == tagged_str => {
                                originate(override::TaggedKey(tag.get().to_string()),
                                          SpanOrigin(sp))
                            }
                            ast::MetaNameValue(ref name,
                                               Spanned {
                                                   node: ast::LitStr(ref v, _),
                                                   span: sp,
                                               }) if *name == value_str => {
                                originate(override::ValueKey(v.get().to_string()),
                                          SpanOrigin(sp))
                            }
                            ast::MetaNameValue(ref name,
                                               Spanned {
                                                   span: sp,
                                                   ..
                                               }) if *name == tagged_str ||
                                                     *name == value_str => {
                                diagnostics().err(SpanOrigin(sp),
                                                  "expected string literal");
                                return;
                            }

                            _ => {
                                expected_single_kvp(value[0].span);
                                return;
                            }
                        };

                        match what {
                            Some(ref mut set) => {
                                if !set.insert(new_what) {
                                    // TODO previous keys?
                                    diagnostics().err(SpanOrigin(value[0].span),
                                                      "duplicate tag or value");
                                    return;
                                }
                            }
                            None => {
                                let mut set = HashSet::new();
                                set.insert(new_what);
                                what = Some(set);
                            }
                        }
                    }
                    ast::MetaWord(ref name) |
                    ast::MetaNameValue(ref name, _) if *name == what_str => {
                        diagnostics().err(SpanOrigin(m.span),
                                          "`what` needs a key-value list");
                        return;
                    }
                }
            }   // for list.iter()

            match attr {
                &Some(Originated { node: override::AddMSK, .. }) |
                &Some(Originated { node: override::RemoveMSK, .. }) => {
                    match what {
                        Some(what) => {
                            for w in what.move_iter() {
                                diagnostics().warn(w.origin,
                                                   "unused attribute");
                            }
                        }
                    }
                    match count {
                        Some(ref o) => {
                            diagnostics().warn(o.origin,
                                               "unused attribute");
                        }
                    }
                }
                _ => {
                    // replace operation
                    let what = what
                        .unwrap_or_else(|| {
                            diagnostics()
                                .err(SpanOrigin(span),
                                     "missing required `what` attribute for \
                                      replace operation");
                            HashSet::new()
                        });
                    let v = override::ReplaceMSK {
                        what: what,
                        count: count.map(|c| c.node ),
                    };
                    *attr = Some(originate(v, SpanOrigin(span)));
                }
            }   // match attr.multi_set_op
        }   // ast::MetaList(_, ref list)
    }   // match item
}

static VERSION_ATTR: &'static str = "rocket_version";
// the version we expect from a compiled build crate.
static VERSION: uint = 0;

fn check_version(krate: &ast::Crate) {
    use syntax::attr::AttrMetaMethods;
    let vers: Vec<&ast::Attribute> = krate.attrs.iter()
        .filter(|attr| attr.check_name(VERSION_ATTR) )
        .collect();
    if vers.len() > 1 {
        for i in vers.move_iter() {
            diagnostics().err(SpanOrigin(i.span),
                              "multiple Rocket version attributes provided");
        }
        return;
    } else if vers.len() == 0 {
        diagnostics().err(DefaultOrigin,
                          format!("missing \\#![rocket_version = {}]",
                                  VERSION_ATTR));
        return;
    }
    let meta = vers[0].node.value;
    match meta.node {
        ast::MetaNameValue(_, ref v) => {
            match v.node {
                ast::LitInt(ver, ast::UnsuffixedIntLit(ast::Plus)) if VERSION != ver as uint => {
                    diagnostics()
                        .err(SpanOrigin(v.span),
                             format!("expected `{}`", VERSION));
                    diagnostics()
                        .err(SpanOrigin(vers[0].span),
                             "Rocket is, at the moment, experimental. At present,
                              no backward compatibility is available.");
                    diagnostics()
                        .info(DefaultOrigin,
                              "Backward compatibility will begin at 1.0");
                    diagnostics().fail();
                }
                ast::LitInt(ver, ast::UnsuffixedIntLit(ast::Plus)) if VERSION == ver as uint => (),
                ast::LitStr(..) => {
                    diagnostics()
                        .fatal(SpanOrigin(v.span),
                               "use an unsuffixed integer literal here");
                }
                _ => {
                    diagnostics().err(SpanOrigin(v.span),
                                      "improper rocket_version attribute");
                    diagnostics()
                        .fatal(SpanOrigin(v.span),
                               format!("expected `{}`", VERSION));
                }
            }
        }
        _ => unreachable!(),
    }
}
