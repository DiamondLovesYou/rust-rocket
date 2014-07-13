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

use std::{path, clone};
use std::default::Default;
use std::collections::{EnumSet, TreeMap, HashMap};
use std::collections::enum_set::CLike;
use std::io::IoResult;
use std::sync::Future;
use syntax::crateid::CrateId;
use rustc::driver::config::CrateType;

use railroad::{YardId, CargoKey};
use super::{TargetId, SubtargetId};
use override::{Origin, Overrides};

#[deriving(Encodable, Decodable, Clone, Hash, Eq, Ord, PartialEq)]
pub struct Address {
    prefix: Prefix,
    segments: Vec<Segment>,
    suffix: Suffix,
}
impl Address {
    pub fn from_crate_id(id: CrateId) -> Address {
        Address {
            prefix: EmptyPrefix,
            segments: vec!(CrateSegment(id, None, EnumSet::empty())),
            suffix: EmptySuffix,
        }
    }
    pub fn from_crate_id_with_phase(id: CrateId, phase: CratePhase) -> Address {
        Address {
            prefix: EmptyPrefix,
            segments: vec!(CrateSegment(id, None, {
                        let mut set = EnumSet::empty();
                        set.add(phase);
                        set
                    })),
            suffix: EmptySuffix,
        }
    }
    pub fn from_yard_suffex(yard: YardId) -> Address {
        Address {
            prefix: EmptyPrefix,
            segments: Vec::new(),
            suffix: TrainYardSuffix(yard),
        }
    }
    pub fn with_sub_address(&self, seg: Segment) -> Address {
        let mut this = self.clone();
        this.segments.push(seg);
        this
    }

    // rel must be relative. Ignores self's suffix.
    pub fn join(&self, rel: Address) -> Address {
        assert!(rel.is_relative());
        let segments = rel.segments;
        let suffix = rel.suffix;
        Address {
            prefix: self.prefix.clone(),
            segments: self.segments.iter().chain(segments.move_iter()),
            suffix: suffix,
        }
    }
}
impl Default for Address {
    fn default() -> Address {
        Address {
            prefix: Default::default(),
            segments: Vec::new(),
            suffix: Default::default(),
        }
    }
}
/// Things that don't belong anywhere in an address except at the end.
#[deriving(Encodable, Decodable, Clone, Hash)]
pub enum Suffix {
    EmptySuffix,

    // the cargokey corresponds to the last yard of the address
    TrainCargoKeySuffix(CargoKey),
    TrainYardSuffix(YardId),
    TrainYardCargoKeySuffix(YardId, CargoKey),
}
impl Default for Suffix {
    fn default() -> Suffix {
        EmptySuffix
    }
}
pub trait Suffixable: clone::Clone {
    fn with_yard_cargo_suffix(&self, yard: YardId, cargo: CargoKey) -> Address;
    fn with_yard_suffix(&self, yard: YardId) -> Address;
}
impl Suffixable for Address {
    fn with_yard_cargo_suffix(&self,
                              yard: YardId,
                              cargo: CargoKey) -> Address {
        Address {
            suffex: TrainYardCargoKeySuffix(yard, cargo),
            .. self.clone()
        }
    }
    pub fn with_yard_suffix(&self,
                            yard: YardId) -> Address {
        Address {
            suffex: TrainYardSuffix(yard),
            .. self.clone()
        }
    }
}
#[deriving(Clone, Hash, Eq, Encodable, Decodable)]
pub enum CratePhase {
    CratePluginPhase,
    CrateLinkPhase,
}
impl CLike for CratePhase {
    fn to_uint(&self) -> uint {
        match self {
            CratePluginPhase => 0,
            CrateLinkPhase =>   1,
        }
    }
    fn from_uint(i: uint) -> CratePhase {
        match i {
            0 => CratePluginPhase,
            1 => CrateLinkPhase,
            _ => unreachable!(),
        }
    }
}
pub type CratePhases = EnumSet<CratePhase>;
/// These are mutually exclusive and only make sense at the start of an address
/// Hence, I moved them to their own enum.
#[deriving(Encodable, Decodable, Clone, Hash, Eq)]
pub enum Prefix {
    EmptyPrefix,
    SourcePrefix,
    TargetPrefix(TargetId),
    SubtargetPrefix(TargetId, SubtargetId),
}
pub trait Prefixable {
    fn is_relative(&self) -> bool;
    fn is_absolute(&self) -> bool;
    fn is_source(&self) -> bool;
    fn target_id(&self) -> Option<TargetId>;
    fn subtarget_id(&self) -> Option<SubtargetId>;
}
impl Prefixable for Prefix {
    pub fn is_relative(&self) -> bool {
        !self.is_absolute()
    }
    pub fn is_absolute(&self) -> bool {
        match self {
            &EmptyPrefix => false,
            _ => true,
        }
    }
    pub fn is_source(&self) -> bool {
        match self {
            &SourcePrefix => true,
            _ => false,
        }
    }
    pub fn target_id(&self) -> Option<TargetId> {
        match self.prefix {
            EmptyPrefix | SourcePrefix => None,
            TargetPrefix(target) | SubtargetPrefix(target, _) => Some(target),
        }
    }
    pub fn subtarget_id(&self) -> Option<SubtargetId> {
        match self.prefix {
            SubtargetPrefix(_, subtarget) => Some(subtarget),
            _ => None,
        }
    }
}
impl Prefixable for Address {
    pub fn is_relative(&self) -> bool {
        self.prefix.is_relative()
    }
    pub fn is_absolute(&self) -> bool {
        self.prefix.is_absolute()
    }
    pub fn target_id(&self) -> Option<TargetId> {
        self.prefix.target_id()
    }
    pub fn subtarget_id(&self) -> Option<SubtargetId> {
        self.prefix.subtarget_id()
    }
}

#[deriving(Encodable, Decodable, Clone, Hash, Ord)]
pub enum Segment {
    CrateSegment(CrateId, Option<CrateType>, CratePhases),
    
    // A file in a project utilizing an external build system (EBS).
    // The path is relative to the root of the project referenced in the project
    PathSegment(Path),

    RegexSegment(String),
}

enum Message {
    AddIfRefMsg,
    // if Some(..), send back the post de-reference ref count.
    // used during shutdown to assert there are no more refs.
    DropIfRefMsg(Option<Sender<u64>>),

    GetOverrides(Sender<(Address, Option<Overrides>)>, Address),

}
#[deriving(Encodable, Decodable, Clone)]
pub struct AddressEdges {
    edges: TreeMap<Segment, Origin>,
}
struct AddressGraph {
    verts: Vec<(CrateId, AddressEdges)>,
    edges: Vec<AddressEdges>,
}
// 
struct Addresser {
    src: HashMap<CrateId, Segment>,
    // -l libraries.
    ebs_libs: HashMap<String, Path>,
    graph: AddressGraph,
}
impl Addresser {
    fn save_db(&self) {
    }
}
#[deriving(Clone)]
pub struct AddresserIf {
    chan: Sender<Message>,
}
impl AddresserIf {
    pub fn new() -> AddresserIf {
        
    }
    pub fn load(rustb_path: &Path) -> IoResult<AddresserIf> {
        fail!();
    }

    pub fn replace_edges(&mut self, vert: Address, edges: AddressEdges) {

    }
    // Prefer replace_edges to this
    pub fn add_edge(&mut self, vert: Address, edge: Segment, origin: Origin) {
        
    }

    pub fn overrides(&self, addr: Address) -> Future<(Address, Option<Overrides>)> {
        let (sender, receiver) = channel();
        let msg = GetOverrides(sender, addr);
        self.chan.send(msg);
        Future::from_receiver(receiver)
    }
}
