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
use std::clone::Clone;
use std::{cmp, default, hash};
use serialize::{Encodable, Decodable};
use std::io::Writer;
use std::collections::hashmap::{HashMap, HashSet};
use std::str::MaybeOwned;
use syntax::codemap;
use syntax::crateid::CrateId;
use std::u64;
use url;

use FromStrWithOrigin;

use platform_dep::ToolId;
use address::Address;

pub type Level = u64;


#[deriving(Encodable, Decodable)]
struct Session {
    by_addr: HashMap<Address, Overrides>,
}

/// The origin for an override
#[deriving(Encodable, Decodable, Clone, Eq, PartialEq)]
pub enum Origin {
    DefaultOrigin,
    CliOrigin,
    SpanOrigin(codemap::Span),
}
impl default::Default for Origin {
    fn default() -> Origin {
        DefaultOrigin
    }
}

#[deriving(Encodable, Decodable, Clone, Eq, PartialEq, Default)]
pub struct Originated<T> {
    pub node: T,
    pub origin: Origin,
}
// this is here purely to save key strokes.
pub type Od<T> = Originated<T>;
pub fn originate<T>(v: T, origin: Origin) -> Originated<T> {
    Originated {
        node: v,
        origin: origin,
    }
}

impl<T> Originated<T> {
    pub fn warn_unused<T: Str>(&self, msg: T) {
        use driver::diagnostics;
        diagnostics().warn(self.origin.clone(),
                           msg)
    }

    pub fn map<U>(self, f: |T, &Origin| -> U) -> Originated<U> {
        let Originated {
            node: node,
            origin: ref origin,
        } = self;
        originate(f(node, origin), origin.clone())
    }
}
impl<T: hash::Hash, S: hash::Writer> hash::Hash<S> for Originated<T> {
    fn hash(&self, state: &mut S) {
        state.hash(self.node);
    }
}
impl<T: FromStrWithOrigin> FromStrWithOrigin for Originated<T> {
    fn from_str_with_origin(s: &str, origin: Origin) -> Option<Originated<T>> {
        let that: Option<T> =
            FromStrWithOrigin::from_str_with_origin(s, origin.clone());
        that.map(|that| originate(that, origin) )
    }
}

#[deriving(Encodable, Decodable, Clone, Eq, PartialEq, Hash)]
pub enum SetOp<T> {
    AddSK,
    RemoveSK,
    // Note this implicitly includes self, ie the value that replaces.
    ReplaceSK(HashSet<Od<TagOrValueKey<T>>>),
}
impl<T> default::Default for SetOp<T> {
    fn default() -> SetOp<T> {
        ReplaceSK(HashSet::new())
    }
}

pub static MULTI_SET_OP_ATTR: &'static str = "multi_set_op";
pub static MULTI_SET_POS_ATTR: &'static str = "multi_set_pos";

#[deriving(Encodable, Decodable)]
pub enum MultiSetOp<T> {
    AddMSK,
    RemoveMSK,
    ReplaceMSK {
        what: HashSet<Od<TagOrValueKey<T>>>,
        count: Option<u64>,
    },
}
impl<T> default::Default for MultiSetOp<T> {
    fn default() -> MultiSetOp<T> {
        AddMSK
    }
}

#[deriving(Encodable, Decodable, Eq, Clone, Hash, Ord, PartialEq)]
pub enum TagOrValueKey<T> {
    TaggedKey(String),
    ValueKey(T),
}

#[deriving(Encodable, Decodable, Eq, Clone, Hash, Ord, PartialEq)]
pub enum MultiSetPosition<T> {
    BeforeMSP(TagOrValueKey<T>),
    AfterMSP(TagOrValueKey<T>),
}
#[deriving(Encodable, Decodable, Clone, Eq, PartialEq)]
pub struct MultiSet<T> {
    pub op: MultiSetOp<T>,
    pub pos: Option<MultiSetPosition<T>>,
}

#[deriving(Encodable, Decodable)]
pub enum CrateSource {
    FileCrateSource(Path),
    // like a -L path
    DirCrateSource(Path),

    RepoCrateSource(url::Url),
    AddressCrateSource(Address),
}
#[deriving(Encodable, Decodable)]
pub struct ValueOrigin<T> {
    origin: Origin,
    level:  u64,
    value:  T,
}
impl<T: Clone> Clone for ValueOrigin<T> {
    fn clone(&self) -> ValueOrigin<T> {
        ValueOrigin {
            origin: self.origin.clone(),
            level:  self.level,
            value:  self.value.clone(),
        }
    }
}
impl<T: cmp::Ord> cmp::Ord for ValueOrigin<T> {
    fn cmp(&self, other: &ValueOrigin<T>) -> cmp::Ordering {
        if self.level == other.level {
            self.value.cmp(other.value)
        } else {
            self.level.cmp(other.level)
        }
    }
}
impl<S: Writer, T: hash::Hash> hash::Hash<S> for ValueOrigin<T> {
    fn hash(&self, state: &mut S) {
        self.level.hash(state);
        self.value.hash(state);
    }
}

pub trait HasEmbedded<T> {
    fn propagate_level(&self) -> T;
}

impl<T> HasEmbedded<ValueOrigin<T>> for ValueOrigin<ValueOrigin<T>> {
    pub fn propagate_level(&self) -> ValueOrigin<T> {
        ValueOrigin {
            level: self.level.checked_add(self.value.level).unwrap_or(u64::MAX),
            .. self.value.clone()
        }
    }
}
impl<T> HasEmbedded<Vec<ValueOrigin<T>>> for ValueOrigin<Vec<ValueOrigin<T>>> {
    pub fn propagate_level(&self) -> Vec<ValueOrigin<T>> {
        self.value.iter().map(|v| {
            ValueOrigin {
                level: self.level.checked_add(v.level).unwrap_or(u64::MAX),
                .. v.clone()
            }
        }).collect()
    }
}

pub enum CfgValue {
    CfgValueNone,
    CfgValueInt(i64),
    CfgValueStr(MaybeOwned<'static>),
    CfgValueFloat(f64),
    CfgValueBool(bool),
    CfgValueVec(Vec<CfgValue>),
}
pub struct CfgMap(String, Origin, CfgValue);


#[deriving(Clone)]
pub type ArgumentValue  = ValueOrigin<MultiSet<String>>;
pub type CrateValue     = ValueOrigin<CrateSource>;
pub struct ArgumentValues(Vec<ArgumentValue>);
impl ArgumentValues {
    pub fn new() -> ArgumentValues {
        Vec::new()
    }

    fn sort(&mut self) {
        let &ArgumentValues(ref mut inner) = self;
        let mut str_idx = self.arg_order
            .iter()
            .enumerate()
            .map(|&(i, arg)| (arg, i) )
            .collect();

        str_idx.sort();
        self.sorted_order = str_idx
            .move_iter()
            .map(|&(_, i)| i as uint)
            .collect();
    }
}
impl Overridable for ArgumentValues {
    fn override_with(&self, o: &ValueOrigin<ArgumentValues>) -> ArgumentValues {
        let mut new = ArgumentValues::new();
        let o = o.propagate_level();
        let mut args = self
            .iter()
            .chain(o.iter())
            .enumerate()
            .map(|&(i, arg)| (arg, i) )
            .collect();
        args.sort();
        let args = args;
        
        fn get<'a>((arg, _): (&'a ArgumentValue, uint)) -> &'a ArgumentValue {
            arg
        }

        let mut iter = args.iter().peekable();
        while iter.peek().is_some() {
            let level = get(iter.peek().unwrap()).level;
            while iter.peek().is_some() && get(iter.peek().unwrap()).level == level {
                let vk_start = iter.clone();
                let vk_end = vk_start.clone().nth();
                let (arg, pos) = iter.next().unwrap();
                /*match arg.value {
                    
                }*/
            }
        }
    }
}

pub static TOOL_ARG_KEY: &'static str = "tool_arg";
pub static TOOL_KEY:     &'static str = "tool";
pub static CRATE_KEY:    &'static str = "crate";
pub static CFG_KEY:      &'static str = "cfg";
pub static CODEGEN_KEY:  &'static str = "codegen";

// The key of the override.
#[deriving(Encodable, Decodable, Hash, Clone, Eq, PartialEq)]
pub enum Override {
    ArgOverride(ToolId, MultiSet<String>),
    ToolOverride(ToolId),
    CrateOverride(Address, SetOp<String>),
    RustCfgOverride,
    CodegenOverride,
}

#[deriving(Encodable, Decodable, Hash, Clone)]
pub struct Overrides {
    overrides_for: Address,
    // the key is the argument mask (typically the program to which the argument applies),
    // the value is the override itself.
    args: HashMap<ToolId, ArgumentValues>,
    tools: HashMap<ToolId, Path>,
    crates: HashMap<String, Path>,

    cfg: Vec<ValueOrigin<MultiSet<CfgValue>>>,
    
    //codegen: CodegenOverrides,
}
impl Overrides {
    pub fn new() -> Overrides {
        Overrides {
            args: HashMap::new(),
            crates: HashMap::new(),
            //codegen: CodegenOverrides::new(),
        }
    }

    pub fn override_argument(&mut self,
                             tool: ToolId,
                             override: ArgumentValue) {
        // TODO after build crate stuffs.
        /*fn mutate(_, &ArgumentValues(ref mut overrides), override) {
            match override.op {
                ReplaceMSK
            }
            let os = replace(overrides, Vec::new());
            let mut replaced = false;
            os.move_iter()
                .filter_map(|o| {
                    let mut move = false;
                    match o.op {
                        Repla
                    }
                });
        }

        use std::mem::replace;
        self.args.mangle(tool,
                         override,
                         |_, override| vec!(override),
                         mutate);*/
    }
    pub fn override_crate(&mut self, krate: CrateSource) {
        
    }
}
impl default::Default for Overrides {
}
impl Overridable for Overrides {
    fn override_with(&self, or: Overrides) -> Overrides {
        let mut new = Overrides::new();
        
        for (k, v) in self.args.iter() {
            match or.args.find(k) {
                None => new.args.insert(k, v),
                Some(ref av) => {
                    
                    
                }
            }
        }
    }
}

pub trait Overridable {
    fn override_with(&self, with: &ValueOrigin<Self>) -> Self;
}

// Declare a macro that will define all CodegenOptions fields and parsers all
// at once. The goal of this macro is to define an interface that can be
// programmatically used by the option parser in order to initialize the struct
// without hardcoding field names all over the place.
//
// The goal is to invoke this macro once with the correct fields, and then this
// macro generates all necessary code. The main gotcha of this macro is the
// cgsetters module which is a bunch of generated code to parse an option into
// its respective field in the struct. There are a few hand-written parsers for
// parsing specific types of values in this module.
//
// Note all fields are wrapped in an Option
macro_rules! codegen_overrides(
    ($($opt:ident : $t:ty = ($init:expr, $parse:ident, $desc:expr)),* ,) =>
(
    #[deriving(Clone)]
    pub struct CodegenOverrides { $($opt: Option<$t>),* }

    impl CodegenOverrides {
        pub fn fill_in_defaults(&mut self) {
            $(if self.$opt.is_none() { self.$opt = Some($init); } )*
        }
    }
    impl Overridable for CodegenOverrides {
        pub fn override_with(&self, overrides: &CodegenOverrides) -> CodegenOverrides {
            CodegenOverrides {
                $($opt: if overrides.$opt.is_none() { self.$opt.clone() }
                  else { overrides.$opt.clone() }),*
            }
        }
    }

    impl default::Default for CodegenOverrides {
        fn default() -> CodegenOverrides {
            CodegenOverrides {
                $($opt: None),*
            }
        }
    }

    pub type CodegenSetter = fn(&mut CodegenOptions, v: Option<&str>) -> bool;
    pub static CG_OPTIONS: &'static [(&'static str, CodegenSetter,
                                      &'static str)] =
        &[ $( (stringify!($opt), cgsetters::$opt, $desc) ),* ];

    mod cgsetters {
        use super::CodegenOverrides;

        $(
            pub fn $opt(cg: &mut CodegenOverrides, v: Option<&str>) -> bool {
                $parse(&mut cg.$opt, v)
            }
        )*

        fn parse_bool(slot: &mut bool, v: Option<&str>) -> bool {
            match v {
                Some(..) => false,
                None => { *slot = true; true }
            }
        }

        fn parse_opt_string(slot: &mut Option<~str>, v: Option<&str>) -> bool {
            match v {
                Some(s) => { *slot = Some(s.to_owned()); true },
                None => false,
            }
        }

        fn parse_string(slot: &mut ~str, v: Option<&str>) -> bool {
            match v {
                Some(s) => { *slot = s.to_owned(); true },
                None => false,
            }
        }

        fn parse_list(slot: &mut ~[~str], v: Option<&str>) -> bool {
            match v {
                Some(s) => {
                    for s in s.words() {
                        slot.push(s.to_owned());
                    }
                    true
                },
                None => false,
            }
        }

    }
) )
