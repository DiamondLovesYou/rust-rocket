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

use std::intrinsics::type_id;
use std::collections::hashmap::HashMap;
use time;

// Determines the maximum size of in-memory objects;
// types larger than 2 MB are deemed freight.
static OCCUPANCY_CAPACITY: u64 = 1024 * 1024 * 2;

#[deriving(Encodable, Decodable)]
pub struct Router {
    router_for: address::Address,
    yards: Vec<Yard>,
    names: HashMap<StrId, uint>,
}
impl Router {
    pub fn embark(&mut self,
                  input: InputCargo,
                  mut conductor: session::Router) -> stats::Router {
        let mut stats = stats::Router::new();
        if self.yards.len() == 0 { return stats; }

        let mut yard_iter = self.yards.iter().peekable();
        let mut train = Train::new(conductor.input);

        let mut next_hump = yard_iter.peek().get_ref().hump_cache.clone();
        let mut this_hump;

        loop {
            let this = match yard_iter.next() {
                Some(n) => n,
                None => { break; }
            };

            let yard_addr = addr.with_yard_suffex(this.id);

            // write to disk cargo written in this yard, but isn't read by the next yard.
            let prep = match yard_iter.peek() {
                Some(ref next) => {
                    let this_reads = this.hump_cache.reads_ref();
                    let this_writes = this.hump_cache.writes_ref();
                    let this_in_mem = this_reads.union(this_writes).collect();

                    let next_reads = next.hump_cache.reads_ref();

                    let write_to_disk = this_writes
                        .difference(next_reads)
                        .filter_map();
                    let read_from_disk = next_reads.difference(this_in_mem);

                    let this_combined = this_prep.reads.union(this_prep.writes);
                    let to_load = next_prep.reads.difference(this_combined);
                    // if we write a cargo in this yard but it isn't used in the next yard,
                    // we still need to keep it in memory
                    let writes_to_unload;
                    let to_unload = this_combined.difference(next_prep.reads);

                    // FIXME check for preexistence with UnloadClass

                    Future::from_val((to_load, to_unload))
                }
                None => {}
            };
            let start = time::precise_time_ns();

            

            yard.route(self, conductor, train);
            let end = time::precise_time_ns();
            
            // first, to free up memory, unload those cargos which aren't used in the next yard.
            let (load, unloaded_writes, unload) = prep.get();

            // first, send the built (written) cargo off the the cache:
            // a part of this step involves calculating whether or not a cargo has changed.

            // now, if a piece of cargo is determined to be dissimilar to its previous value,
            // bump the revisions of said cargo:

            
        }
    }

    fn prep_for_next_yard(first_iteration: bool,
                          yard: &Yard,
                          prep: YardPrep) -> Future<YardPrep> {
        
        if first_iteration {
        } else {
            
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
    fn new() -> Hump {
        let mut hump = Hump {
            by_cargo_key: HashMap::new(),
            writes:       HashSet::new(),
            reads:        HashSet::new(),
        };
    }
    fn merge_read(&mut self, id: CargoKey) -> bool {
        let mut new = false;
        self.by_cargo_key.insert_or_update_with(id, ScannedClass, |_, c| {
                // don't dirty the new status if this yard also loaded the cargo
                if c.is_read() || *c == LoadClass {
                    return;
                }
                match c {
                    &NonStopClass => *c = ScannedClass,
                    _ => { return; }
                };
                new = true;
            });
        if new {
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
                new = true;
            });
        if new {
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
pub type HumpPush = |CargoKey, Classification, Origin|;
pub type Hump = fn(push: HumpPush);
pub type YardId = StrId;
#[deriving(Encodable, Decodable)]
pub struct Yard {
    origin: Origin,
    id: YardId,
    hump: Hump,
    hump_cache: HumpCache,
    industry: Industry,
}
pub type Industry = fn(&session::Router, &mut Train) -> Diversion;
impl Yard {
    pub fn new(id: YardId,
               origin: Origin,
               industry: Industry,
               hump: Hump) -> Yard {
        Yard {
            origin: origin,
            id: id,
            hump: hump,
            industry: industry,
            hump_cache: None,
        }
    }
    fn route(&self, sess: &session::Session_, router: &Router, train: &mut Train) {
        
    }
}

#[deriving(Eq, Clone, Encodable, Decodable, Hash)]
enum CargoKey {
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

enum Car {
    DecoupledCar(Origin),
    CoupledCar(Origin, Box<Any>),
    UnpackingCar(Future<Box<Any>>),
    UnpackedCar(Box<Any>),
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
    fn new(input: Input) -> Train {
        let mut train = Train {
            cars: HashMap::new(),
        };
        train.couple(input);
        train
    }
    pub fn by_type_id<'train, T>(&'train self) -> CarRef<'train, T> {
        self.hump.ensure_class()
    }
    pub fn by_override<'train, T>(&'train self) -> CarRef<'train, T> {
        
    }
    /// only available to the first yard.
    pub fn main_input<'a>(&'a self) -> &'a Option<InputCargo> {
        self.hump.merge_read(MainInputCargoKey);
        self.main_input
    }
    pub fn decouple<T: Car>(&mut self, origin: Origin) -> T {
        let key = TypeIdCargoKey(type_id::<T>());
        self.cars.pop(key).expect("yard hump didn't declare read \
                                             access to this car")
    }
    pub fn couple<T: Car>(&mut self, origin: Origin, car: T) {
        let key = TypeIdCargoKey(type_id::<T>());
        self.cars.insert_or_update_with(key, box car as Box<Any>, |_, old| {
                
            });
    }
    pub fn clone_car<T: Car>(&self) -> T {
        self.cars.find_copy(type_of::<T>()).expect("yard hump didn't specify \
                                                   read access to this car")
    }
}
type BorrowFlag = uint;
static UNUSED: BorrowFlag = 0;
static WRITING: BorrowFlag = -1;

pub struct CarRef<'train, TCargo> {
    cargo: TCargo,
    access: Classification,
    flags: uint,
}
pub struct Ref<'train, 'car, TCargo> {
    parent: &'car CarRef<'train, TCargo>,
}
impl<'train, 'car, TCargo> Deref<TCargo> for Ref<'train, 'car, TCargo> {
    fn deref<'a>(&'a self) -> &'a TCargo {
        &self.parent.cargo
    }
}
                                       
