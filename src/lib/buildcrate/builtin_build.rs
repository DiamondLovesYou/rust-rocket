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
// lib.rs or a main.rs in all sub directories of src in the project root. This
// behaviour will be overriden just by having a build crate in the project.

pub fn new_default() {
}

pub fn build<T: SubtargetEsk>(target: &SubtargetEsk, krate: &Path, out: &Path) {
    let mut bk = BuildCrate {
        ts: 0,
        src_paths: Vec::new(),

        path: build_crate_path.clone(),

    };

    let mut rc_sess = build_session({
        let mut opts = basic_options();
        opts.crate_types = vec!(CrateTypeDylib);
        opts.target_triple = target.triple().to_str();
        opts.debuginfo = FullDebugInfo;
        opts
    },
                                    Some(build_crate_path.clone()));
    let cfg = build_configuration(&sess);
    let input = FileInput(build_crate_path.clone());
    let krate = phase_1_parse_input(&sess, cfg, &input);

    // TODO: ask Cargo to resolve extern crates used by the build crate.
    let outputs = OutputFilenames {
        out_directory: out.dir_path(),
        out_filestem: "dylib",
        single_output_file: Some(out.clone()),
    };
    let mut loader = Loader::new(&rc_sess);
    let id = link::find_crate_id(krate.attrs.as_slice(),
                                 outputs.out_filestem.as_slice());
    let (krate, ast_map) =
        phase_2_configure_and_expand(&rc_sess,
                                     &mut loader,
                                     krate,
                                     &id);
    let ast_map = AstMap::new(rc_sess, id.clone(), cfg.clone());
    let krate = ast_map.fold_crate(krate);

    check_version(sess, &krate);
}
#[deriving(Clone, Encodable, Decodable, Hash, Eq)]
pub enum OverrideValue {
	ArgOverrideValue(String),
	ToolOverrideValue(Path),
	LibraryOverrideValue(String),
	RustCfgOverrideValue(String),
	CodegenOverrideValue(String),
}
#[deriving(Clone, Encodable, Decodable, Hash, Eq)]
pub enum Item_ {
    OverrideItem {
        key: override::Key,
        value: OverrideValue,
    },
}
impl Item_ {
    pub fn new((kind_str, kind_span): (&str, Span), (key_str, key_span): (&str, Span), item: &ast::Item) -> Item_ {
	fn fatal_unknown_item(item: &ast::Item) -> ! {
	    diagnostics().fatal(SpanOrigin(item.span),
				"unknown item");
	}
	let key: override::Key;
	let value: override::Value;
        match kind_str {
	    "tool" => {
		let toolid: platform_dep::ToolId;
		match FromStr::from_str(key_str) {
		    Some(k) => {
			toolid = k;
		    }
		    None => {
			diagnostics().fatal(SpanOrigin(key_span),
					    "unknown tool");
		    }
		}
		match item.item {
		    ItemStatic() => {
			value = ToolOverrideValue(/* something */);
		    }
		    _ => {
			
		    }
		}
	    }
	    "tool-arg" => {
		let toolid: platform_dep::ToolId;
		match FromStr::from_str(key_str) {
		    Some(tid) => {
			toolid = tid;
		    }
		    None => {
			diagnostics().fatal(SpanOrigin(key_span),
					    "unknown tool");
		    }
		}
		match item.item {
		    ItemStatic() => {
			value = ArgOverrideValue(/* something */);
		    }
		    None => {
			fatal_unknown_item(item);
		    }
		}
	    }
	}
    }
}

#[deriving(Clone, Encodable, Decodable, Hash, Eq)]
pub struct Item {
    node: NodeId,
    addr: Address,
    span: Span,
    override: Item_,
}
struct AstMap<'a> {
    // we need to
    get_version_inserted: bool,
    sess: &'a Session,
    // this isn't used for any macro expanding, it's just here because it
    // provide a large number of helpful functions to generate ast.
    ext: ext::base::ExtCtxt<'a>,
    id_to_item: HashMap<NodeId, Item>,
}

pub struct AttributeMeta {
    addrs: Option<Vec<(String, Span)>>,
    level: Option<(uint, Span)>,
    multi_set_kind: Option<(String, Span)>,
    multi_set_position: Option<(String, Span)>,
    override: Option<(String, String, Span)>,
}

pub fn expect_word<'a>(meta: Gc<MetaItem>) -> String {
    match meta.value {
        MetaWord(ref word) => {
            word.get().clone()
        }
        _ => {
            diagnostics().
        }
    }
}

impl<'a> AstMap<'a> {
    pub fn new(sess: &'a Session, cid: CrateId, cfg: CrateConfig) -> AstMap<'a> {
        let ecfg = syntax::ext::expand::ExpansionConfig {
            deriving_hash_type_parameter: sess.features.default_type_params.get(),
            crate_id: cid,
        };
        AstMap {
            get_version_inserted: false,
            sess: sess,
            ext: ExtCtxt::new(&sess.parse_sess, cfg, ecfg),         
        }
    }
    fn filter_item(&mut self, item: &ast::Item) -> bool {
        let mut attr = AttributeMeta {
            addrs: None,
            level: None,
            multi_set_kind: None,
            multi_set_position: None,
        };

        let metas = item.attrs
            .filter(|attr| !attr.is_sugared_doc )
            .filter_map(|attr| attr.value )
            .filter_map(|value| {
                match value.node {
                    MetaList(ref name, ref meta)
                        if name == InternedString::new("rocket") =>
                        Some(meta)
                        _ => None,
                }
            })
            .flat_map(|metalist| metalist.iter());
        let addr_attr = InternedString::new("address");
        let level_attr = InternedString::new("level");
        let multi_set_kind_attr = InternedString::new("multi_set_op");
        let multi_set_position_attr =
            InternedString::new("multi_set_pos");
        let override_attr = InternedString::new("override");
        for @MetaItem {
            span: span,
            node: node,
        } in metas.move_iter() {
            match node {
                MetaNameValue(ref name, ref value) if name == addr_attr => {
                    match attrs.addr {
                        Some(ref mut addrs) => {
                            addrs.push((value, node.span));
                        }
                        None => {
                            attrs.addr = Some(vec!((value, span)))
                        }
                    }
                }
                MetaNameValue(ref name, ref value) if name == level_attr => {
                    if attrs.level.is_some() {
                        diagnostics().error(SpanOrigin(span),
                                            "multiple level attributes \
                                            provided");
                    }
                    match from_str(value) {
                        Some(level) => {
                            attrs.level = Some((level, span));
                        }
                        None => {
                            diagnostics().error(SpanOrigin(span),
                                                format!("expected integer; \
                                                        found `{}`",
                                                        value).as_slice());
                        }
                    }
                }
                MetaNameValue(ref name, ref value) if name == multi_set_kind => {
                    if attrs.multi_set_kind.is_some() {
                        diagnostics().error(SpanOrigin(span),
                                            "multiple multi_set_ops \
                                            attributes provided");
                    }
                    attrs.multi_set_kind = Some((value.clone(), span));
                }
                MetaNameValue(ref name, ref value) if name == multi_set_position => {
                    if attrs.multi_set_position.is_some() {
                        diagnostics().error(SpanOrigin(span),
                                            "multiple multi_set_pos \
                                             attributes provided");
                    }
                    attrs.multi_set_position = Some((value.clone(),
                                                     span));
                }
                MetaNameValue(ref name, ref value)
                    if name == override => {
                    if attrs.override.is_some() {
                        diagnostics().error(SpanOrigin(span),
                                            "multiple overrides \
                                             not permitted");
                    }
                    attrs.override = Some((value.clone(),
                                           span));
                }
                _ => {
                    diagnostics().error(SpanOrigin(span),
                                        "unknown attribute");
                }
            }
        }
        if attr.addrs.is_none() {
            diagnostics().error(SpanOrigin(item.span),
                                "missing address attribute");
        }
        if attr.override.is_none() {
            diagnostics().error(SpanOrigin(item.span),
                                "missing override attribute");
        }
        let item = Item {
            span: item.span,
            item: match attr.override {
		Some((ref kind, ref key, _)) => {
		    Item_::new(kind, key)
		}
	    }
        };
    }

    fn get_compiled_version_stmt(&self) -> Stmt {
        dummy_spanned(box(GC) StmtExpr(self.ext.expr_uint(DUMMY_SP,
                                                          super::COMPILED_VERSION)))
    }
}
impl fold::Folder for AstMap {
    fn fold_mod(&mut self, module: &ast::Mod) -> ast::Mod {
        let filtered_items: Vec<&@ast::Item> = m.items.iter()
            .filter(|&a| self.filter_item(*a))
            .collect();

        let mut flattened_items = filtered_items.move_iter()
            .flat_map(|&x| self.fold_item(x).move_iter())
            .collect();

        if !self.get_version_inserted {
            self.get_version_inserted = true;

            let get_version = ast::Item {
                id: self.sess.next_node_id(),
                // Note: this invalid identifier is deliberate.
                ident: Ident::new(intern("rocket-get_version")),
                node: ItemFn(ast::P(ast::FnDecl {
                    cf: ast::Returns,
                    output: self.ext.ty_ident(mk_sp(0, 0),
                                              Ident::new(intern("i32"))),
                    inputs: Vec::new(),
                    variadic: false,
                }),
                             ast::NormalFn,
                             abi::System,
                             ast::Generics {
                                 lifetimes: Vec::new(),
                                 ty_params: OwnedSlice::empty(),
                             },
                             ast::P(Block {
                                 view_items: Vec::new(),
                                 stmts: vec!(self.get_compiled_version_stmt()),
                             })),
                vis: ast::Public,
                span: DUMMY_SPAN,
            };

            flattened_items.push(get_version);
        }

        let filtered_view_items = m.view_items
            .iter()
            .filter_map(|a| {
                filter_view_item(cx, a).map(|x| cx.fold_view_item(x))
            })
            .collect();

        ast::Mod {
            inner: m.inner,
            view_items: filtered_view_items,
            items: flattened_items
        }
    }
    // TODO visit extern crate deps.
}

pub static EXPANSION_YARD_NAME: &'static str = "expansion";
pub static GATED_FEATURE_CHECKING_YARD_NAME: &'static str = "gated feature checking";

pub fn new_router_for_crate(sess: &session::Session_) -> Router {
    use rustc;
    use rustc::front;
    
    let parse_input_yard = Yard::new(&"parse input".to_strbuf(),
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
