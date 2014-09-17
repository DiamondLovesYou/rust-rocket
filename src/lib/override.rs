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
use std::hash::{Hash};
use std::hash::sip::SipState;
use serialize::{Encodable, Decodable, Encoder, Decoder};
use std::io::Writer;
use std::collections::hashmap::{HashMap, HashSet};
use std::str::MaybeOwned;
use syntax::codemap;
use syntax::crateid::CrateId;
use std::u64;
use url;
use uuid::Uuid;

use FromStrWithOrigin;

use address::Address;
use buildcrate::BuildCrateId;
use driver::diagnostics::Driver;
use platform_dep::ToolId;

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
impl<S: hash::Writer> hash::Hash<S> for Origin {
    fn hash(&self, state: &mut S) {
        // no-op
    }
}

#[deriving(Encodable, Decodable, Clone, Hash, Eq, PartialEq, Default)]
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
impl<T: FromStrWithOrigin> FromStrWithOrigin for Originated<T> {
    fn from_str_with_origin(s: &str, origin: Origin) -> Option<Originated<T>> {
        let that: Option<T> =
            FromStrWithOrigin::from_str_with_origin(s, origin.clone());
        that.map(|that| originate(that, origin) )
    }
}

#[deriving(Encodable, Decodable, Clone)]
pub enum SetOp<T: Eq + hash::Hash> {
    AddSK,
    RemoveSK,
    // Note this implicitly includes self, ie the value that replaces.
    ReplaceSK(HashSet<Od<TagOrValueKey<T>>>),
}
impl<T: Eq + PartialEq + hash::Hash> default::Default for SetOp<T> {
    fn default() -> SetOp<T> {
        ReplaceSK(HashSet::new())
    }
}
impl<T: Eq + PartialEq + hash::Hash> Eq for SetOp<T> { }
impl<T: Eq + PartialEq + hash::Hash> PartialEq for SetOp<T> {
    fn eq(&self, rhs: &SetOp<T>) -> bool {
        match (self, rhs) {
            (&AddSK, &AddSK) => true,
            (&RemoveSK, &RemoveSK) => true,
            (&ReplaceSK(ref lset),
             &ReplaceSK(ref rset)) if lset.len() == rset.len() => {
                let (lower, _) = lset.symmetric_difference(rset)
                    .size_hint();
                lower == 0
            }
            _ => false
        }
    }
}
impl<T: hash::Hash<SipState> + Eq> hash::Hash<SipState> for SetOp<T> {
    fn hash(&self, state: &mut SipState) {
        match self {
            &AddSK => MULTI_SET_OP_ADD_TAG.hash(state),
            &RemoveSK => MULTI_SET_OP_REMOVE_TAG.hash(state),
            &ReplaceSK(ref what) => {
                MULTI_SET_OP_REPLACE_TAG.hash(state);
                // we assume a stable seed.
                for origin in what.iter() {
                    origin.hash(state);
                }
            }
        }
    }
}

pub static MULTI_SET_OP_ATTR: &'static str = "multi_set_op";
pub static MULTI_SET_POS_ATTR: &'static str = "multi_set_pos";

static MULTI_SET_OP_ADD_TAG: u32 = 0;
static MULTI_SET_OP_REMOVE_TAG: u32 = 1;
static MULTI_SET_OP_REPLACE_TAG: u32 = 2;

#[deriving(Clone)]
pub enum MultiSetOp<T> {
    AddMSK,
    RemoveMSK,
    ReplaceMSK {
        what: HashSet<Od<TagOrValueKey<T>>>,
        // None means replace all
        count: Option<u64>,
    },
}
impl<T> default::Default for MultiSetOp<T> {
    fn default() -> MultiSetOp<T> {
        AddMSK
    }
}
impl<T: Eq + PartialEq + hash::Hash> Eq for MultiSetOp<T> { }
impl<T: Eq + PartialEq + hash::Hash> PartialEq for MultiSetOp<T> {
    fn eq(&self, rhs: &MultiSetOp<T>) -> bool {
        match (self, rhs) {
            (&AddMSK, &AddMSK) => true,
            (&RemoveMSK, &RemoveMSK) => true,
            (&ReplaceMSK {
                what: ref lset,
                count: ref lcount,
            },
             &ReplaceMSK {
                 what: ref rset,
                 count: ref rcount,
             }) if lcount == rcount && lset.len() == rset.len() => {
                let (lower, _) = lset.symmetric_difference(rset)
                    .size_hint();
                lower == 0
            }
            _ => false
        }
    }
}
impl<T: hash::Hash<SipState> + Eq + PartialEq> hash::Hash<SipState> for MultiSetOp<T> {
    fn hash(&self, state: &mut SipState) {
        match self {
            &AddMSK => MULTI_SET_OP_ADD_TAG.hash(state),
            &RemoveMSK => MULTI_SET_OP_REMOVE_TAG.hash(state),
            &ReplaceMSK {
                what: ref what,
                count: ref count
            } => {
                MULTI_SET_OP_REPLACE_TAG.hash(state);
                // we assume a stable seed.
                for origin in what.iter() {
                    origin.hash(state);
                }
                count.hash(state);
            }
        }
    }
}
impl<T: Decodable<D, E> + Eq + Hash, E, D: Decoder<E>> Decodable<D, E> for MultiSetOp<T> {
    fn decode(d: &mut D) -> Result<MultiSetOp<T>, E> {
        let tag: u32 = try!(d.read_u32());
        match tag {
            MULTI_SET_OP_ADD_TAG => Ok(AddMSK),
            MULTI_SET_OP_REMOVE_TAG => Ok(RemoveMSK),
            MULTI_SET_OP_REPLACE_TAG => {
                let what = try!(d.read_map(|d, _| Decodable::decode(d) ));
                let count = try!(d.read_option(|d, is_null| {
                    if !is_null {
                        Ok(Some(try!(d.read_u64())))
                    } else {
                        Ok(None)
                    }
                }));
                Ok(ReplaceMSK {
                    what: what,
                    count: count,
                })
            }
            _ => unreachable!(),
        }
    }
}
impl<T: Encodable<S, E> + Eq + Hash, E, S: Encoder<E>> Encodable<S, E> for MultiSetOp<T> {
    fn encode(&self, s: &mut S) -> Result<(), E> {
        match self {
            &AddMSK => s.emit_u32(MULTI_SET_OP_ADD_TAG),
            &RemoveMSK => s.emit_u32(MULTI_SET_OP_REMOVE_TAG),
            &ReplaceMSK {
                what: ref what,
                count: ref count,
            } => {
                try!(s.emit_u32(MULTI_SET_OP_REPLACE_TAG));
                let mut iter = what.iter();
                try!(s.emit_map(what.len(), |s| {
                    iter.next().unwrap().encode(s)
                }));
                match count {
                    &Some(v) => s.emit_option_some(|s| v.encode(s) ),
                    &None    => s.emit_option_none(),
                }
            }
        }
    }
}

#[deriving(Encodable, Decodable, Eq, Clone, Hash, Ord, PartialOrd, PartialEq)]
pub enum TagOrValueKey<T> {
    TaggedKey(String),
    ValueKey(T),
}

#[deriving(Encodable, Decodable, Eq, Clone, Hash, Ord, PartialOrd, PartialEq)]
pub enum MultiSetPosition<T> {
    BeforeMSP(TagOrValueKey<T>),
    AfterMSP(TagOrValueKey<T>),
}
#[deriving(Encodable, Decodable, Clone)]
pub struct MultiSet<T: Eq + hash::Hash> {
    pub op: MultiSetOp<T>,
    pub pos: Option<MultiSetPosition<T>>,
}
impl<T: Eq + PartialEq + hash::Hash> Eq for MultiSet<T> { }
impl<T: Eq + PartialEq + hash::Hash> PartialEq for MultiSet<T> {
    fn eq(&self, rhs: &MultiSet<T>) -> bool {
        self.pos == rhs.pos && self.op == rhs.op
    }
}
impl<T: hash::Hash<SipState> + Eq> hash::Hash<SipState> for MultiSet<T> {
    fn hash(&self, state: &mut SipState) {
        self.op.hash(state);
        self.pos.hash(state);
    }
}

#[deriving(Encodable, Decodable, Clone, Eq, PartialEq, Hash)]
pub enum CrateSource {
    FileCrateSource(Path),
    // like a -L path
    DirCrateSource(Path),

    // RepoCrateSource expects an URL
    RepoCrateSource(String),
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
impl<T: Eq> Eq for ValueOrigin<T> { }
impl<T: PartialEq> PartialEq for ValueOrigin<T> {
    fn eq(&self, rhs: &ValueOrigin<T>) -> bool {
        self.level == rhs.level && self.value == rhs.value
    }
}
impl<T: cmp::Ord> cmp::Ord for ValueOrigin<T> {
    fn cmp(&self, other: &ValueOrigin<T>) -> cmp::Ordering {
        if self.level == other.level {
            self.value.cmp(&other.value)
        } else {
            self.level.cmp(&other.level)
        }
    }
}
impl<T: Ord + PartialOrd> cmp::PartialOrd for ValueOrigin<T> {
    fn partial_cmp(&self, rhs: &ValueOrigin<T>) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}
impl<T: hash::Hash<S>, S: hash::Writer> hash::Hash<S> for ValueOrigin<T> {
    fn hash(&self, state: &mut S) {
        self.level.hash(state);
        self.value.hash(state);
    }
}

pub trait HasEmbedded<T> {
    fn propagate_level(self) -> T;
    fn get_level(&self) -> u64;
}
/*impl<T> HasEmbedded<ValueOrigin<T>> for ValueOrigin<T> {
    fn propagate_level(self) -> ValueOrigin<T> {
        self
    }
}
impl<T: HasEmbedded<U>, U> HasEmbedded<T> for ValueOrigin<T> {
    fn propagate_level(self) -> ValueOrigin<T> {
        ValueOrigin {
            level: self.level
                .checked_add(&self.value.get_level())
                .unwrap_or(u64::MAX),
            .. self.value
        }
    }
    fn get_level(&self) -> u64 {
        self.level
    }
}
impl<T: HasEmbedded<U>, U> HasEmbedded<Vec<T>> for ValueOrigin<Vec<T>> {
    fn propagate_level(self) -> Vec<T> {
        self.value.move_iter().map(|v| {
            ValueOrigin {
                level: self.level
                    .checked_add(&v.get_level())
                    .unwrap_or(u64::MAX),
                .. v
            }
        }).collect()
    }
    fn get_level(&self) -> u64 {
        self.level
    }
}*/

// TODO: use CfgNameValue instead of all these.
#[deriving(Encodable, Decodable, Clone, PartialEq)]
pub enum CfgValue {
    CfgValueNone,
    CfgValueInt(i64),
    CfgValueStr(String),
    CfgValueFloat(f64),
    CfgValueBool(bool),
    CfgValueVec(Vec<CfgMap>),
}
impl Eq for CfgValue {}
impl<S: hash::Writer> Hash<S> for CfgValue {
    fn hash(&self, state: &mut S) {
        match self {
            &CfgValueNone => 0u32.hash(state),
            &CfgValueInt(v) => {
                1u32.hash(state);
                v.hash(state);
            }
            &CfgValueStr(v) => {
                2u32.hash(state);
                v.hash(state);
            }
            &CfgValueFloat(v) => {
                3u32.hash(state);
                // Yikes.
                v.to_string().hash(state);
            }
            &CfgValueBool(v) => {
                4u32.hash(state);
                v.hash(state);
            }
            &CfgValueVec(ref v) => {
                5u32.hash(state);
                for i in v.iter() {
                    i.hash(state);
                }
            }
        }
    }
}
#[deriving(Encodable, Decodable, Clone, Hash, Eq, PartialEq)]
pub struct CfgMap(String, Origin, CfgValue);

#[deriving(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub enum ArgumentAddressType {
    StaticAddrType(String),
    // TODO
}
pub struct ArgumentValue {
    build_crate_id: Uuid,
}

pub struct ArgumentValues(Vec<ArgumentValue>);
impl ArgumentValues {
    pub fn new() -> ArgumentValues {
        ArgumentValues(Vec::new())
    }

    /*fn sort(&mut self) {
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
    }*/
}
impl Slice<ArgumentValue> for ArgumentValues {
    fn as_slice<'a>(&'a self) -> &'a [ArgumentValue] {
        let &ArgumentValues(ref inner) = self;
        inner.as_slice()
    }
}
/*impl Overridable for ArgumentValues {
    fn override_with(&self, o: &ValueOrigin<ArgumentValues>) -> ArgumentValues {
        let mut new = ArgumentValues::new();
        let mut args: Vec<(&ArgumentValue, uint)> = self.as_slice()
            .iter()
            .chain(o.value.as_slice().iter())
            .enumerate()
            .map(|(i, arg)| (arg, i) )
            .collect();
        //args.sort();
        let args = args;
        
        fn get<'a>((arg, _): &&(&'a ArgumentValue, uint)) -> &'a ArgumentValue {
            arg
        }

        let mut iter = args.iter().peekable();
        while iter.peek().is_some() {
            let level = get(iter.peek().unwrap()).level;
            while iter.peek().is_some() && get(iter.peek().unwrap()).level == level {
                let vk_start = iter.clone();
                let vk_end = vk_start.clone().nth();
                let (arg, pos) = iter.next().unwrap();
                match arg.value {
                    
                }
            }
        }
    }
}*/

pub static TOOL_ARG_KEY: &'static str = "tool_arg";
pub static TOOL_KEY:     &'static str = "tool";
pub static CRATE_KEY:    &'static str = "crate";
pub static CFG_KEY:      &'static str = "cfg";
pub static CODEGEN_KEY:  &'static str = "codegen";

// The key of the override.
#[deriving(Encodable, Decodable, Hash, Clone, Eq, PartialEq)]
pub enum Key {
    ArgKey(ToolId),
    ToolKey(ToolId),
    CrateKey(Address),
    CfgKey,
    CodegenKey,
}

#[deriving(Encodable, Decodable, Hash, Clone, Eq, PartialEq)]
pub enum ValueType {
    // A &'static str string.
    StringType,

    // TODO
    FnType,
}

#[deriving(Encodable, Decodable, Clone, Eq, PartialEq)]
pub struct Value {
    pub override: Override,
    pub build_crate: BuildCrateId,
    pub symbol: String,
    pub value: ValueType,
}
impl hash::Hash<SipState> for Value {
    fn hash(&self, s: &mut SipState) {
        self.override.hash(s);
        self.build_crate.hash(s);
        self.symbol.hash(s);
        self.value.hash(s);
    }
}

#[deriving(Encodable, Decodable, Clone, Eq, PartialEq)]
pub enum Override {
    ArgOverride(ToolId, MultiSet<String>),
    ToolOverride(ToolId),
    CrateOverride(Address, SetOp<String>),
    RustCfgOverride,
    CodegenOverride,
}
impl hash::Hash<SipState> for Override {
    fn hash(&self, s: &mut SipState) {
        static ARG_TAG: u32 = 0;
        static TOOL_TAG: u32 = 1;
        static CRATE_TAG: u32 = 2;
        static CFG_TAG: u32 = 3;
        static CODEGEN_TAG: u32 = 4;

        match self {
            &ArgOverride(ref id, ref set) => {
                ARG_TAG.hash(s);
                id.hash(s);
                set.hash(s);
            }
            &ToolOverride(ref id) => {
                TOOL_TAG.hash(s);
                id.hash(s);
            }
            &CrateOverride(ref addr, ref set) => {
                CRATE_TAG.hash(s);
                addr.hash(s);
                set.hash(s);
            }
            &RustCfgOverride => CRATE_TAG.hash(s),
            &CodegenOverride => CODEGEN_TAG.hash(s),
        }
    }
}

#[deriving(Encodable, Decodable, Clone)]
pub struct Overrides {
    overrides_for: Address,
    // the key is the argument mask (typically the program to which the argument
    // applies), the value is the override itself.
    args: HashMap<ToolId, MultiSet<String>>,
    tools: HashMap<ToolId, Path>,
    crates: HashMap<String, Path>,

    cfgs: Vec<ValueOrigin<MultiSet<CfgValue>>>,
    
    //codegen: CodegenOverrides,
}

impl hash::Hash<SipState> for Overrides {
    fn hash(&self, state: &mut SipState) {
        self.overrides_for.hash(state);
        for arg in self.args.iter() {
            arg.hash(state);
        }
        for tool in self.tools.iter() {
            tool.hash(state);
        }
        for krate in self.crates.iter() {
            krate.hash(state);
        }
        for cfg in self.cfgs.iter() {
            cfg.hash(state);
        }
    }
}

impl Overrides {
    pub fn new(for_addr: Address) -> Overrides {
        Overrides {
            overrides_for: for_addr,
            args: HashMap::new(),
            tools: HashMap::new(),
            crates: HashMap::new(),
            cfgs: Vec::new(),
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
    fn default() -> Overrides {
        use driver::address;
        Overrides::new(address().clone())
    }
}
impl Overridable for Overrides {
    fn override_with(&self, or: &ValueOrigin<Overrides>) -> Overrides {
        let mut new: Overrides = default::Default::default();
        
        for (k, v) in self.args.iter() {
            match or.value.args.find(k) {
                None => {
                    new.args.insert(k.clone(), v.clone());
                }
                Some(ref av) => {
                    
                    
                }
            }
        }
        new
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
