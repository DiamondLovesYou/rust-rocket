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

use std::any::Any;
use std::cell::{RefCell};
use std::collections::{HashMap, HashSet};
use std::intrinsics::{type_id, TypeId};
use std::sync::Future;
use std::default;
use std::default::Default;

use time;
use rustc;
use sqlite;

use address;
use driver::database::{EnumVariantToKey, EnumVariantKey,
                       ToBindArg};
use driver::session;
use override;
use override::Origin;
use stats;

// Determines the maximum size of in-memory objects;
// types larger than 2 MB are deemed freight.
static OCCUPANCY_CAPACITY: u64 = 1024 * 1024 * 2;

#[deriving(Encodable, Decodable, Clone, Hash, Eq, PartialEq, Ord, PartialOrd)]
pub struct RouterId(pub u64);

#[deriving(Encodable, Decodable)]
pub struct Router {
    id: RouterId,
    name: String,
    yards: Vec<Yard>,
}
impl Router {
    pub fn new() -> Router {
        Router {
            yards: Vec::new(),
        }
    }

    pub fn push_yard(&mut self,
                     mut yard: Yard) -> YardId {
        let id = self.yards.len();
        yard.id = id;
        self.yards.push(yard);
        id
    }

    pub fn embark(&self,
                  input: InputCargo,
                  mut conductor: session::Router) {
        let addr = conductor.address().clone();
        let mut yard_iter = self.yards.iter().peekable();
        let mut train = Train::new(conductor.input);

        let mut next_hump = yard_iter.peek().get_ref().hump_cache.clone();
        let mut this_hump;

        for yard in yard_iter {
            let yard_addr = conductor.with_yard_suffex(yard.id);

            //let start = time::precise_time_ns();
            yard.route(self, conductor, train);
            //let end = time::precise_time_ns();

            // if a piece of cargo is determined to be dissimilar to its previous value,
            // bump the revisions of said cargo:
        }
    }
}
#[deriving(Clone, Hash, Encodable, Decodable)]
pub enum InputCargo {
    CrateFileInputCargo(Path),

    // a subdirectory to a project which has an external build.
    ExternalBuildInputCargo(Path),
}
#[deriving(Clone, Hash, Eq)]
pub enum Diversion {
    NoDiversion,
    // use a Vec of zero elements to halt.
    PermDiversion(Vec<Yard>),
    TempDiversion(Vec<Yard>),
}
impl default::Default for Diversion {
    fn default() -> Diversion { NoDiversion }
}

#[deriving(Encodable, Decodable, Clone, Eq, Hash)]
pub enum Classification {
    // unused for the time being, until I tackle Yard access classifications
    NonStopClass,

    // read only.
    ScannedClass,

    // read/write.
    RefineryClass,

    // the cargo is added to the train. this implies the cargo was
    // never on the train to begin with.
    LoadClass,
    // the cargo is removed from the train and not added back.
    UnloadClass,
}
impl Classification {
    pub fn is_read(&self) -> bool {
        match self {
            NonStopClass => false,
            ScannedClass => true,
            RefineryClass => true,
            LoadClass => false,
            UnloadClass => true,
        }
    }
    pub fn is_write(&self) -> bool {
        match self {
            NonStopClass => false,
            ScannedClass => false,
            RefineryClass => true,
            LoadClass => true,
            UnloadClass => false,
        }
    }
}

#[deriving(Encodable, Decodable, Clone)]
pub struct HumpCache {
    by_cargo_key: HashMap<CargoKey, Classification>,
    // here, Classification is either LoadClass (for writes) or UnloadClass (for reads).
    writes:       HashSet<CargoKey>,
    reads:        HashSet<CargoKey>,
}
impl HumpCache {
    fn new() -> HumpCache {
        let mut hump = HumpCache {
            by_cargo_key: HashMap::new(),
            writes:       HashSet::new(),
            reads:        HashSet::new(),
        };
    }
    fn merge_read(&mut self, id: CargoKey) -> bool {
        let mut is_new = false;
        self.by_cargo_key.insert_or_update_with(id, ScannedClass, |_, c| {
                // don't dirty the new status if this yard also loaded the cargo
                if c.is_read() || *c == LoadClass {
                    return;
                }
                match c {
                    &NonStopClass => *c = ScannedClass,
                    _ => { return; }
                };
                is_new = true;
            });
        if is_new {
            self.reads.insert(id);
            true
        } else { false }
    }
    fn merge_write(&mut self, id: CargoKey) -> bool {
        let mut is_new = false;
        self.by_cargo_key.insert_or_update_with(id, LoadClass, |_, c| {
                if c.is_write() {
                    return;
                }
                match c {
                    &NonStopClass | &ScannedClass => *c = RefineryClass,
                    _ => { return }
                };
                is_new = true;
            });
        if is_new {
            self.writes.insert(id);
            true
        } else { false }
    }
    fn ensure_class(&mut self, id: CargoKey, class: Classification) {
        self.by_cargo_key.insert_or_update_with(id, class, |_, c| {
                c = class;
            });

        let set = if class.is_read() {
            self.reads
        } else if class.is_write() {
            self.writes
        };
        set.push(id);
    }
    fn reads_ref<'a>(&'a self) -> &'a HashSet<CargoKey> {
        self.reads
    }
    fn writes_ref<'a>(&'a self) -> &'a HashSet<CargoKey> {
        self.writes
    }
    pub fn find_reads(&self) -> HashSet<CargoKey> {
        self.reads.clone()
    }
    pub fn find_writes(&self) -> HashSet<CargoKey> {
        self.writes.clone()
    }
}
//pub type HumpPush = |CargoKey, Classification|;
pub type Hump = fn(push: |CargoKey, Classification|);
pub type YardId = u32;

static DUMMY_YARD_ID: u32 = 0;

#[deriving(Encodable, Decodable)]
pub struct Yard {
    id: YardId,
    origin: Origin,
    pub name: String,
    hump: Hump,
    hump_cache: HumpCache,
    industry: Industry,
}
pub type Industry = fn(&session::Router, &mut Train) -> Diversion;
impl Yard {
    pub fn new(name: String,
               origin: Origin,
               industry: Industry,
               hump: Hump) -> Yard {
        Yard {
            id: DUMMY_YARD_ID,
            origin: origin,
            name: name,
            hump: hump,
            hump_cache: Default::default(),
            industry: industry,
            hump_cache: None,
        }
    }
    fn route(&self, router: &Router, train: &mut Train) {

    }
}

#[deriving(Eq, PartialEq, Ord, PartialOrd, Clone, Encodable, Decodable, Hash)]
pub enum CargoKey {
    TypeIdCargoKey(TypeId),
    OverrideCargoKey(override::Key),

    // build steps/what-have-you
    DepCargoKey(address::Address),

    // files
    FileCargoKey(address::Address),

    // i.e. large files. these are never loaded into memory.
    FreightCargoKey(address::Address),

    // the Router's main input.
    MainInputCargoKey,
}
impl EnumVariantToKey<EnumVariantKey> for CargoKey {
    fn enum_variant_key(&self) -> Option<EnumVariantKey> {
        static TYPEID_KEY: EnumVariantKey = 0;
        static OVERRIDE_KEY: EnumVariantKey = 1;
        static DEP_KEY: EnumVariantKey = 2;
        static FILE_KEY: EnumVariantKey = 3;
        static FREIGHT_KEY: EnumVariantKey = 4;

        match self {
            &MainInputCargoKey => None,
            &TypeIdCargoKey(_) => Some(TYPEID_KEY),
            &OverrideCargoKey(_) => Some(OVERRIDE_KEY),
            &DepCargoKey(_) => Some(DEP_KEY),
            &FileCargoKey(_) => Some(FILE_KEY),
            &FreightCargoKey(_) => Some(FREIGHT_KEY),
        }
    }
}
enum Car {
    DecoupledCar(Origin),
    CoupledCar(Origin, Box<Any + 'static>),
    UnpackingCar(Future<Box<Any + 'static>>),
    UnpackedCar(Box<Any + 'static>),
}

#[deriving(Encodable, Decodable, Hash, Clone, Eq, PartialEq, Ord, PartialOrd)]
pub struct TrainId(pub u64);

impl ToBindArg for TrainId {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        self.inner_id().to_bind_arg()
    }
}
impl<'a> ToBindArg for &'a TrainId {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        self.inner_id().to_bind_arg()
    }
}

impl TrainId {
    fn inner_id(&self) -> u64 {
        let &TrainId(id) = self;
        id
    }

    pub fn filename(&self) -> Path {
        static QUERY: &'static str =
            "SELECT input FROM [trains] WHERE id == ?1";
    }
    pub fn name(&self) -> String {
        static QUERY: &'static str =
            "SELECT name FROM [trains] WHERE id == ?1";
        let mut names = database()
            .select(QUERY,
                    [self.inner_id().to_bind_arg()],
                    |c| {
                        c.get_text()
                            .map(|slice| slice.to_string() )
                            .unwrap()
                    });
        assert_eq!(names.len(), 1);
        names.swap_remove(0)
            .unwrap()
    }
}

#[deriving(Encodable, Decodable, Hash)]
pub struct Train {
    cars: HashMap<CargoKey, Car>,
    hump: RefCell<Hump>,

    // to ease the transition to rustb (since this is used everywhere in rustc).
    // heuristics are employed to detect changes.
    rustc_sess: rustc::driver::session::Session,
}
impl Train {
    fn new(input: InputCargo) -> Train {
        let mut train = Train {
            cars: HashMap::new(),
        };
        train.couple(input);
        train
    }

    pub fn sess<'a>(&'a self) -> &'a rustc::driver::session::Session {
        &self.rustc_sess
    }
    pub fn by_type_id<'train, T>(&'train self) -> CarRef<'train, T> {
        self.hump.ensure_class()
    }
    pub fn by_override<'train, T>(&'train self, override: override::Key, _: Option<T>) -> CarRef<'train, T> {

    }

    /// only available to the first yard.
    pub fn main_input<'a>(&'a self) -> &'a Option<Path> {
        self.hump.merge_read(MainInputCargoKey);
        self.main_input
    }
    pub fn decouple<T: Any>(&mut self) -> T {
        let key = TypeIdCargoKey(type_id::<T>());
        self.cars.pop(key).expect("yard hump didn't declare read \
                                   access to this car") as T
    }
    pub fn couple<T: Any>(&mut self, car: T) {
        let key = TypeIdCargoKey(type_id::<T>());
        self.cars.insert_or_update_with(key, box car as Box<Any>, |_, old| {

            });
    }
    pub fn clone_car<T: Any>(&self) -> T {
        self.cars.find_copy(type_id::<T>()).expect("yard hump didn't specify \
                                                    read access to this car")
    }
}
/*impl rustc::driver::driver::SessionCarrier for Train {
    fn sess<'a>(&'a self) -> &'a rustc::driver::session::Session {
        &self.rustc_sess
    }
}*/

type BorrowFlag = uint;
static UNUSED: BorrowFlag = 0;
static WRITING: BorrowFlag = -1;

pub struct CarRef<'train, TCargo> {
    cargo: TCargo,
    access: Classification,
    flags: uint,
}
impl<'train, TCargo> Deref<TCargo> for CarRef<'train, TCargo> {
    fn deref<'a>(&'a self) -> &'a TCargo {
        assert!(self.access.is_read());
        &self.cargo
    }
}
