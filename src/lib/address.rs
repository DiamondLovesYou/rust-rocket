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

use serialize::{Encodable, Decodable, Encoder, Decoder};
use std::{path, clone};
use std::collections::{EnumSet, TreeMap, HashMap};
use std::collections::enum_set::CLike;
use std::default::Default;
use std::from_str::FromStr;
use std::io::IoResult;
use std::sync::Future;
use sqlite;
use rustc::driver::config::CrateType;

use {CrateId, TargetId, SubtargetId};
use driver;
use driver::database::database;
use driver::database::{ToBindArg};
use railroad::{YardId, TrainId, CargoKey};
use override::{Origin, Overrides};

#[deriving(Encodable, Decodable, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct Address {
    id: AddressId,
    prefix: Prefix,
    edges: Vec<TrainId>,
}
impl Address {
    pub fn new(prefix: Prefix, edges: &[TrainId]) -> Address {
        assert!(edges.len() > 0);
        static EXISTING_QUERY: &'static str =
            "SELECT id FROM addrs \
             WHERE prefix_enum == ?1 AND \
                   prefix_target == ?2 AND \
                   prefix_subtarget == ?3 AND \
                   first_edge_id == ?4";
        static INSERT: &'static str = "INSERT INTO [addrs] (\
                prefix_enum, prefix_target, prefix_subtarget) \
                VALUES(?1, ?2, ?3, NULL)";
        static EDGE_INSERT: &'static str = "INSERT INTO [addr_edges] (\
                addr_id, vert_id, next_edge_id)
                VALUES(?1, ?2, ?3)";
        static ADDR_FIRST_EDGE_UPDATE: &'static str = "UPDATE [addrs] \
                SET first_edge_id = ?1 WHERE id = ?2";

        let db = database();
        let _ = db.savepoint();

        let params = [prefix.to_sql_tag().to_bind_arg(),
                      prefix.target_id().to_bind_arg(),
                      prefix.subtarget_id().to_bind_arg(),
                      edges.head().to_bind_arg()];

        let existing = db.prepare(EXISTING_QUERY,
                   |cursor| {
                       assert_eq!(cursor.bind_params(params),
                                  sqlite::SQLITE_OK);
                       let code = cursor.step();
                       if code == sqlite::SQLITE_DONE {
                           None
                       } else if code == sqlite::SQLITE_ROW {
                           Some(AddressId(cursor.get_i64(0) as u64))
                       } else {
                           unreachable!()
                       }
                   });

        match existing {
            Some(id) => Address {
                id: id,
                prefix: prefix,
                edges: edges.iter().map(|v| v.clone()).collect(),
            },
            None => {
                let id = AddressId(db.insert(INSERT,
                                             params));
                let mut last_edge_id = None;
                let edges = edges.iter()
                    .rev()
                    .map(|edge| {
                        let params = [id.to_bind_arg(),
                                      edge.to_bind_arg(),
                                      last_edge_id.to_bind_arg()];
                        let id = TrainId(db.insert(EDGE_INSERT,
                                                   params));
                        last_edge_id = Some(id);
                        id
                    })
                    .rev()
                    .collect();
                assert_eq!(db.update(ADDR_FIRST_EDGE_UPDATE,
                                     [id.to_bind_arg(),
                                      last_edge_id.unwrap().to_bind_arg()]),
                           1);
                Address {
                    id: id,
                    prefix: prefix,
                    edges: edges,
                }
            },
        }
    }
    pub fn with_sub_address(self, seg: TrainId) -> Address {
        let Address {
            id: _,
            edges: self_edges,
            prefix: self_prefix,
        } = self;
        let edges: Vec<TrainId> = self_edges
            .move_iter()
            .chain(Some(seg).move_iter())
            .collect();
        Address::new(self_prefix, edges.as_slice())
    }
    pub fn with_source(self, src: TrainId) -> Address {
        let Address {
            id: _,
            edges: self_edges,
            prefix: _,
        } = self;
        let edges: Vec<TrainId> = self_edges
            .move_iter()
            .chain(Some(src).move_iter())
            .collect();
        Address::new(SourcePrefix, edges.as_slice())
    }

    fn insert_into_db(self) -> Address {
        let Address {
            id: _,
            edges: edges,
            prefix: prefix,
        } = self;

        Address::new(prefix, edges.as_slice())
    }

    // rel must be relative. Ignores self's suffix.
    pub fn join_addr(self, rel: Address) -> Address {
        assert!(rel.is_relative());
        let Address {
            id: _,
            edges: rel_edges,
            prefix: _,
        } = rel;
        let Address {
            id: _,
            edges: self_edges,
            prefix: self_prefix,
        } = self;
        let edges: Vec<TrainId> = self_edges
            .move_iter()
            .chain(rel_edges.move_iter())
            .collect();
        Address::new(self_prefix, edges.as_slice())
    }
}

impl ::FromStrWithOrigin for Address {
    fn from_str_with_origin(s: &str, origin: Origin) -> Option<Address> {
        // TODO
        unimplemented!()
    }
}

/// the inner u64 must be public so address!() macros can work
#[deriving(Encodable, Decodable, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct AddressId(pub u64);

impl ToBindArg for AddressId {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        let &AddressId(ref inner) = self;
        inner.to_bind_arg()
    }
}

impl AddressId {
    fn inner_id(&self) -> u64 {
        let &AddressId(inner) = self;
        inner
    }
    pub fn get(&self) -> Address {
        static PREFIX_QUERY: &'static str =
            "SELECT prefix_enum, prefix_target_id, \
                    prefix_subtarget_id, first_edge_id \
             FROM addrs WHERE id == ?1";
        static EDGES_QUERY: &'static str =
            "WITH RECURSIVE addr_chain(train_id) AS (\
               SELECT vert_id FROM [addrs_edges] WHERE id == ?1 \
               UNION ALL \
               SELECT vert_id FROM [addr_chain] \
                  WHERE id == addr_chain.next_edge_id) \
             SELECT train_id FROM [addr_chain]";
        let prefix = database()
            .prepare(PREFIX_QUERY,
                     |cursor| {
                         assert_eq!(cursor.bind_params([sqlite::Integer64(self.inner_id() as i64)]),
                                    sqlite::SQLITE_OK);
                         assert_eq!(cursor.step(),
                                    sqlite::SQLITE_ROW);
                         match cursor.get_int(0) {
                             PREFIX_EMPTY_TAG => EmptyPrefix,
                             PREFIX_SOURCE_TAG => SourcePrefix,
                             PREFIX_TARGET_TAG => {
                                 TargetPrefix(TargetId(cursor.get_i64(1) as u64))
                             }
                             PREFIX_SUBTARGET_TAG => {
                                 SubtargetPrefix(TargetId(cursor.get_i64(1) as u64),
                                                 SubtargetId(cursor.get_i64(2) as u64))
                             }
                             _ => unreachable!(),
                         }
                     });

        let edges = database()
            .prepare(EDGES_QUERY,
                     |c| {
                         assert_eq!(c.bind_params([sqlite::Integer64(self.inner_id() as i64)]),
                                    sqlite::SQLITE_OK);
                         let mut edges: Vec<TrainId> = Vec::new();
                         loop {
                             if c.step() != sqlite::SQLITE_ROW {
                                 break;
                             }
                             edges.push(TrainId(c.get_i64(0) as u64));
                         }
                         edges
                     });
        Address {
            id: self.clone(),
            edges: edges,
            prefix: prefix,
        }
    }
}

#[deriving(Clone, Hash, Eq, PartialEq, Ord, PartialOrd, Encodable, Decodable)]
pub enum CratePhase {
    CratePluginPhase,
    CrateLinkPhase,
}
impl CLike for CratePhase {
    fn to_uint(&self) -> uint {
        match self {
            &CratePluginPhase => 0,
            &CrateLinkPhase =>   1,
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

static PREFIX_EMPTY_TAG: int = 0;
static PREFIX_SOURCE_TAG: int = 1;
static PREFIX_TARGET_TAG: int = 2;
static PREFIX_SUBTARGET_TAG: int = 3;

/// These are mutually exclusive and only make sense at the start of an address
/// Hence, I moved them to their own enum.
#[deriving(Encodable, Decodable, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub enum Prefix {
    EmptyPrefix,
    SourcePrefix,
    TargetPrefix(TargetId),
    SubtargetPrefix(TargetId, SubtargetId),
}
impl Prefix {
    fn to_sql_tag(&self) -> int {
        match self {
            &EmptyPrefix => PREFIX_EMPTY_TAG,
            &SourcePrefix => PREFIX_SOURCE_TAG,
            &TargetPrefix(..) => PREFIX_TARGET_TAG,
            &SubtargetPrefix(..) => PREFIX_SUBTARGET_TAG,
        }
    }
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
        match self {
            &EmptyPrefix | &SourcePrefix => None,
            &TargetPrefix(target) | &SubtargetPrefix(target, _) => Some(target),
        }
    }
    pub fn subtarget_id(&self) -> Option<SubtargetId> {
        match self {
            &SubtargetPrefix(_, subtarget) => Some(subtarget),
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
    pub fn is_source(&self) -> bool {
        self.prefix.is_source()
    }
    pub fn target_id(&self) -> Option<TargetId> {
        self.prefix.target_id()
    }
    pub fn subtarget_id(&self) -> Option<SubtargetId> {
        self.prefix.subtarget_id()
    }
}
impl Default for Prefix {
    fn default() -> Prefix {
        EmptyPrefix
    }
}
