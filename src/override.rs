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
use std::cmp::TotalOrd;
use std::clone::Clone;
use std::default;
use serialize::{Encodable, Decodable};
use std::io::Writer;
use collection::hashmap::{HashMap, HashSet};

pub type Level = u64;


#[deriving(Encodable, Decodable)]
struct Session {
    by_addr: HashMap<address::Address, Overrides>,
}

/// The origin for an override
#[deriving(Encodable, Decodable, Clone)]
pub enum Origin {
    DefaultOrigin,
    CliOrigin,
    SpanOrigin(codemap::Span),
}
#[deriving(Encodable, Decodable)]
pub enum MultiSetKind {
    AddMSK,
    RemoveMSK,
}
#[deriving(Encodable, Decodable, Eq, Clone, Hash)]
pub enum MultiSetPosition<T> {
    DontCareMSP,
    BeforeMSP(T),
    AfterMSP(T),
}
#[deriving(Encodable, Decodable)]
pub enum MultiSetValue<T: Encodable + Decodable> {
    ReplaceMultiSetValue(MultiSetKind, T, T),
    
    FlagMultiSetValue(MultiSetKind, MultiSetPosition<T>, T),
}
impl<T: cmp::TotalOrd> cmp::TotalOrd for MultiSetValue<T> {
    fn cmp(&self, other: &MultiSetValue<T>) -> cmp::Ordering {
        match match self {
            &ReplaceMultiSetValue(_, ref key, _) |
                &FlagMultiSetValue(_, ref key) => {
                match other {
                    &ReplaceMultiSetValue(_, ref key2, _) |
                        &FlagMultiSetValue(_, _, ref key2) => key.cmp(key2)
                }
            }
        } {
            cmp::Equal => {
                match self {
                    &ReplaceMultiSetValue(RemoveMultiSetKind, ..) => {
                        match other {
                            &ReplaceMultiSetValue(RemoveMultiSetKind, ..) => Equal,
                            _ => Less,
                        }
                    }
                    _ => Greater,
                }
            }
            order => order,
        }
    }
}

#[deriving(Encodable, Decodable)]
pub enum CrateSource {
    FileCrateSource(Path),
    /// like a -L path
    DirCrateSource(Path),

    RepoCrateSource(repo::Url),
}
#[deriving(Encodable, Decodable)]
pub struct Value<T: Encodeable + Decodable> {
    origin: Origin,
    level:  u64,
    value:  T,
}
impl<T: Clone> Clone for Value<T> {
    fn clone(&self) -> Value<T> {
        Value {
            origin: self.origin.clone(),
            level:  self.level,
            value:  self.value.clone(),
        }
    }
}
impl<T: cmd::TotalOrd> cmp::TotalOrd for Value<T> {
    fn cmp(&self, other: &Value<T>) -> cmp::Ordering {
        if self.level == other.level {
            self.value.cmp(other.value)
        } else {
            self.level.cmp(other.level)
        }
    }
}
impl<S: Writer, T: hash::Hash> Hash<S> for Value<T> {
    fn hash(&self, state: &mut S) {
        self.level.hash(state);
        self.value.hash(state);
    }
}

pub trait HasEmbedded<T> {
    fn propagate_level(&self) -> T;
}

impl<T> HasEmbedded<Value<T>> for Value<Value<T>> {
    pub fn propagate_level(&self) -> Value<T> {
        Value<T> {
            level: self.level.checked_add(self.value.level).unwrap_or(u64::MAX),
            .. self.value.clone()
        }
    }
}
impl<T> HasEmbedded<~[Value<T>]> Value<~[Value<T>]> {
    pub fn propagate_level(&self) -> ~[Value<T>] {
        self.value.iter().map(|v| {
                Value<T> {
                    level: self.level.checked_add(v.level).unwrap_or(u64::MAX),
                    .. v.clone()
                }
            }).collect()
    }
}
pub enum CfgValue {
    CfgValueNone,
    CfgValueInt(i64),
    CfgValueStr(SendStr),
    CfgValueFloat(f64),
    CfgValueBool(bool),
    CfgValueVec(Vec<CfgValue>),
}
pub struct CfgMap(~str, Origin, CfgValue);


#[deriving(Clone)]
pub type ArgumentValue  = Value<MultiSetValue<~str>>;
pub type CrateValue     = Value<CrateSourceValue>;
pub type ArgumentValues = ~[ArgumentValue];
impl ArgumentValues {
    pub fn new() -> ArgumentValues {
        ~[]
    }

    fn sort(&mut self) {
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
    fn override_with(&self, o: &Value<ArgumentValues>) -> ArgumentValues {
        let mut new = ArgumentValues::new();
        let o = o.propagate_level();
        let mut args = self
            .iter()
            .chain(o.iter()).
            .enumerate()
            .map(|&(i, arg)| (arg, i) )
            .collect();
        args.sort();
        let args = args;
        
        fn get<'a>((arg, _): (&'a ArgumentValue, uint)) -> &'a ArgumentValue {
            arg
        }

        let mut iter = args.iter().peekable();
        loop iter.peek().is_some() {
            let level = get(iter.peek().unwrap()).level;
            while iter.peek().is_some() && get(iter.peek().unwrap()).level == level {
                let vk_start = iter.clone();
                let vk_end = vk_start.clone().nth();
                let (arg, pos) = iter.next().unwrap();
                match arg.value {
                    ReplaceMultiSetValue(AddMultiSetKind, ..) => {}
                }
            }
        }
    }
}
#[deriving(Encodable, Decodable, Hash, Clone, Eq)]
pub enum Key {
    ArgKey(platform_dep::ToolId),
    ToolKey(platform_dep::ToolId),
    LibraryKey(platform_dep::LibraryId),
    RustCfgKey,
    CodegenKey(~str),
}
pub enum Value {
    ArgValue(ArgumentValues),
    ToolValue(platform_dep::Tool),
    RustCfgValue(Vec<CfgMap>),
}
#[deriving(Encodable, Decodable, Hash, Clone)]
pub struct Overrides {
    /// the key is the argument mask (typically the program to which the argument applies),
    /// the value is the override itself.
    args: HashMap<platform_dep::ToolId, ArgumentValues>,
    tools: HashMap<platform_dep::ToolId, Path>,
    libraries: HashMap<platform_dep::LibraryId, Path>,

    crates: HashMap<~str, CrateOverrideValue>,
    cfg: Vec<Value<MultiSetValue<CfgValue>>>,
    
    codegen: CodegenOverrides,
}
impl Overrides {
    pub fn new() -> Overrides {
        Overrides{
            args: HashMap::new(),
            crates: HashMap::new(),
            codegen: CodegenOverrides::new(),
        }
    }

    pub fn flatten(&mut self) {
        // this is specifically not unimplemented b/c this operation is only an optimization.
        error!("not implemented");
    }

    pub fn override_argument(&mut self, mask: ~str, override: ArgumentValue) {
        self.args.mangle(mask,
                         override,
                         |_, override| ~[override],
                         |_, &mut overrides, override| {
                overrides.push(override)
            });
    }
    pub fn override_crate(&mut self, crate_: CrateOverrideValue) {
        
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
    fn override_with(&self, with: &Value<Self>) -> Self;
}

/// Declare a macro that will define all CodegenOptions fields and parsers all
/// at once. The goal of this macro is to define an interface that can be
/// programmatically used by the option parser in order to initialize the struct
/// without hardcoding field names all over the place.
///
/// The goal is to invoke this macro once with the correct fields, and then this
/// macro generates all necessary code. The main gotcha of this macro is the
/// cgsetters module which is a bunch of generated code to parse an option into
/// its respective field in the struct. There are a few hand-written parsers for
/// parsing specific types of values in this module.
///
/// Note all fields are wrapped in an Option
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
    impl Overridable for CodegenOverrides
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

codegen_overrides!(
    target_cpu: ~str = (~"generic", parse_string,
        "select target processor (llc -mcpu=help for details)"),
    target_feature: ~str = (~"", parse_string,
        "target specific attributes (llc -mattr=help for details)"),
    passes: ~[~str] = (~[], parse_list,
        "a list of extra LLVM passes to run (space separated)"),
    save_temps: bool = (false, parse_bool,
        "save all temporary output files during compilation"),
    cross_path: Option<~str> = (None, parse_opt_string,
        "the path to the Android NDK"),
    rpath: bool = (true, parse_bool,
        "disables setting the rpath in libs/exes"),
    prepopulate_passes: bool = (true, parse_bool,
        "don't pre-populate the pass manager with a list of passes"),
    vectorize_loops: bool = (true, parse_bool,
        "don't run the loop vectorization optimization passes"),
    vectorize_slp: bool = (true, parse_bool,
        "don't run LLVM's SLP vectorization pass"),
    soft_float: bool = (false, parse_bool,
        "generate software floating point library calls"),
    gen_crate_map: bool = (false, parse_bool,
        "force generation of a toplevel crate map"),
    prefer_dynamic: bool = (false, parse_bool,
        "prefer dynamic linking to static linking"),
    integrated_as: bool = (true, parse_bool,
        "use an external assembler rather than LLVM's integrated one"),
)
