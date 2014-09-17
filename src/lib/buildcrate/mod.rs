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

use std::collections::TreeSet;
use std::comm::{Sender, Receiver};
use std::dynamic_lib::DynamicLibrary;
use std::hash;
use std::hash::sip::SipState;
use std::intrinsics::TypeId;
use std::mem::transmute;
use std::path::Path;

use syntax::ast;

use rustc;

use uuid;

use address::Address;
use override::{Origin, DefaultOrigin, SpanOrigin};
use driver::{session, diagnostics};
use driver::diagnostics::Driver;
use BUILD_CRATE_FILENAME;
use {SubtargetEsk, Build, CrateId, TargetId, SubtargetId};

pub mod bootstrap;
pub mod build;
pub mod dylib;
pub mod load;
pub mod rtio_wrapper;

static PROJECT_SOURCE_PATH: &'static str = "src";


#[deriving(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct SymbolId(pub BuildCrateId, pub SymbolIndex);
pub type SymbolIndex = u64;

#[deriving(Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct TypeIdHash(pub u64);

impl<S: hash::Writer> hash::Hash<S> for TypeIdHash {
    fn hash(&self, state: &mut S) {
        use std::hash::Writer;
        use std::mem::transmute_copy;
        let &TypeIdHash(ref h) = self;
        let h = h.to_le();
        let h: [u8, ..8] = unsafe { transmute_copy(&h) };
        state.write(h);
    }
}

#[deriving(Clone, Eq, PartialEq, Hash)]
pub enum SymbolKind {
    StaticStrKind,
    FnSymbolKind(TypeIdHash),
}
impl Ord for SymbolKind {
    fn cmp(&self, rhs: &SymbolKind) -> Ordering {
        match (self, rhs) {
            (&StaticStrKind, &StaticStrKind) => Equal,
            (&StaticStrKind, _) => Less,
            (_, &StaticStrKind) => Greater,

            (&FnSymbolKind(ref ltid),
             &FnSymbolKind(ref rtid))
                if ltid == rtid => Equal,
            (&FnSymbolKind(ref ltid),
             &FnSymbolKind(ref rtid))
                if ltid < rtid => Less,
            (&FnSymbolKind(ref ltid),
             &FnSymbolKind(ref rtid))
                if ltid > rtid => Greater,
        }
    }
}
impl PartialOrd for SymbolKind {
    fn partial_cmp(&self, rhs: &SymbolKind) -> Option<Ordering> {
        Some(self.cmp(rhs))
    }
}

#[deriving(Clone)]
pub struct Symbol {
    id: SymbolId,
    ptr: *const (),
    kind: SymbolKind,
}
impl Symbol {
    pub fn get_str_opt<'a>(&'a self) -> Option<&'a str> {
        match self.kind {
            StaticStrKind => unsafe {
                Some(transmute(self.ptr))
            },
            _ => None,
        }
    }
    pub fn get_fn_opt<R: 'static>(&self) -> Option<R> {
        match self.kind {
            FnSymbolKind(tid) if tid == TypeIdHash(TypeId::of::<R>().hash()) => unsafe {
                Some(transmute(self.ptr))
            },
            _ => None,
        }
    }
}
impl Eq for Symbol {}
impl PartialEq for Symbol {
    fn eq(&self, rhs: &Symbol) -> bool {
        self.id.eq(&rhs.id)
    }
}
impl Ord for Symbol {
    fn cmp(&self, rhs: &Symbol) -> Ordering {
        self.id.cmp(&rhs.id)
    }
}
impl PartialOrd for Symbol {
    fn partial_cmp(&self, rhs: &Symbol) -> Option<Ordering> {
        self.id.partial_cmp(&rhs.id)
    }
}

#[deriving(Encodable, Decodable, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct BuildCrateId(pub u64);

/// This MUST match the bootstrapper's id in master.db.
static BOOTSTRAP_BUILD_CRATE_ID: BuildCrateId = BuildCrateId(1);

impl BuildCrateId {
    pub fn bootstrapper() -> BuildCrateId {
        BOOTSTRAP_BUILD_CRATE_ID.clone()
    }
}

pub struct BuildCrate {
    id: BuildCrateId,
    path: Path,

    target: Option<TargetId>,
    subtarget: Option<SubtargetId>,

    dylib: Option<DynamicLibrary>,
}
impl BuildCrate {
}
impl Eq for BuildCrate {}
impl PartialEq for BuildCrate {
    fn eq(&self, rhs: &BuildCrate) -> bool {
        self.id.eq(&rhs.id)
    }
}
impl Ord for BuildCrate {
    fn cmp(&self, rhs: &BuildCrate) -> Ordering {
        self.id.cmp(&rhs.id)
    }
}
impl PartialOrd for BuildCrate {
    fn partial_cmp(&self, rhs: &BuildCrate) -> Option<Ordering> {
        Some(self.cmp(rhs))
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
