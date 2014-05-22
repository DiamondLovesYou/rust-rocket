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

use std::to_bytes;
use std::path;
use rustc::driver::session::CrateType;

#[deriving(Encodable, Decodable, Clone, Hash, Eq)]
pub struct Address {
    prefix: Prefix,
    segments: Vec<Segment>,
    suffix: Suffix,
}
impl Address {
    pub fn from_crate_id(id: CrateId) -> Address {
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

    /// rel must be relative. Ignores self's suffix.
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
impl default::Default for Address {
    fn default() -> Address {
        Address {
            prefix: default(),
            segments: Vec::new(),
            suffix: default(),
        }
    }
}
/// Things that don't belong anywhere in an address except at the end.
#[deriving(Encodable, Decodable, Clone, Hash)]
pub enum Suffix {
    EmptySuffix,
    /// the cargokey corresponds to the last yard of the address
    TrainCargoKeySuffix(CargoKey),
    TrainYardSuffix(YardId),
    TrainYardCargoKeySuffix(YardId, CargoKey),
}
impl default::Default for Suffix {
    fn default() -> Suffix {
        EmptySuffex
    }
}
pub trait Suffixable: clone::Clone {
    fn with_yard_cargo_suffix(&self, yard: YardId, cargo: CargoKey) -> Address;
    fn with_yard_suffix(&self, yard: YardId) -> Address;
}
impl Suffixable for Address {
    pub fn with_yard_cargo_suffex(&self,
                                  yard: YardId,
                                  cargo: CargoKey) -> Address {
        Address {
            suffex: TrainYardCargoTypeSuffex(yard, cargo),
            .. self.clone()
        }
    }
    pub fn with_yard_suffex(&self,
                            yardid: YardId) -> Address {
        Address {
            suffex: TrainYardSuffex(yard),
            .. self.clone()
        }
    }
}
#[deriving(Clone, Hash, Eq, Encodable, Decodable)]
pub enum CratePhase {
    CrateSyntaxPhase,
    CrateLinkPhase,
}
impl CLike for CratePhase {
    fn to_uint(&self) -> {
        match self {
            CrateSyntaxPhase => 1 << 1,
            CrateLinkPhase =>   1 << 2,
        }
    }
    fn from_uint(i: uint) -> CratePhase {
        match i {
            1 << 1 => CrateSyntaxPhase,
            1 << 2 => CrateLinkPhase,
            _ => unreachable!(),
        }
    }
}
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
    pub fn is_relative(&self) -> bool;
    pub fn is_absolute(&self) -> bool;
    pub fn is_source(&self) -> bool;
    pub fn target_id(&self) -> Option<TargetId>;
    pub fn subtarget_id(&self) -> Option<SubtargetId>;
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

#[deriving(Encodable, Decodable, Clone, Hash)]
pub enum Segment {
    CrateSegment(CrateId, Option<CrateType>, enum_set::EnumSet<CratePhase>),
    
    /// A file in a project utilizing an external build system (EBS).
    /// The path is relative to the root of the project referenced by this address.
    PathSegment(PathId),
}