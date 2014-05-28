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

use address::Address;

pub enum Phase {
    SyntaxPhase,
    LinkPhase,
}
enum Message {
    Startup(Port<Message>),
    AddIfRefMessage,
    DropIfRefMessage,
    
    // async
    ReportDepMessage(address::Address,  // suffexed with the yard from whence it came.
                     Dep),
    
}

#[deriving(Encodable, Decodable, Clone, Hash)]
enum Dep {
    PathDep(Path),
    AddressDep(Address),
}
enum Locus {
    // A rustb build
    BuildLocus(Path,   // path to .rustb/
               ~str),  // target name

    
}
struct SystemDb;

struct UserDb {
    crates: HashMap<CrateId, Locus>,
}


#[deriving(Encodable, Decodable)]
struct BuildDb {
    // a.k.a. top->bottom.
    by_addr: HashMap<Address, Vec<Dep>>,
    // a.k.a bottom->top.
    by_dep:  HashMap<Dep, Vec<Address>>,

    // the number of times a resource has been built.
    current_revisions: HashMap<Address, u64>,
}
struct Session {
    db: BuildDb,
    db_file: File,
    
}
impl Session {
    fn task(port: Port<DepMessage>,
            freshness: Freshness,
            db_path: &Path) {
        let mut db = DepDb::load(db_path).unwrap_or_else(|| {
                // create a new db
            });
    }
    pub fn new((port, chan): (Port<Message>, Chan<Message>),
           db: &Path) -> IoResult<MasterIf> {
    }
    
}

pub struct MasterIf {
    chan: Chan<Message>,
}
impl MasterIf {
    pub fn new(db: Path) -> MasterIf {
        Session::new(Chan::new(), db).ok().unwrap()
    }

    pub fn create_sess(&self, addr: address::Address) -> SessionIf {
        SessionIf {
            chan: self.chan.clone(),
            addr: addr,
        }
    }
}
pub struct SessionIf {
    chan: Chan<Message>,
    addr: address::Address,
}
impl clone::Clone for SessionIf {
    fn clone(&self) -> SessionIf {
        self.chan.send(AddIfRefDepMessage);
        SessionIf {
            chan: self.chan.clone(),
            addr: self.addr.clone(),
        }
    }
}
impl drop::Drop for SessionIf {
    fn drop(&mut self) {
        // even if the other side hung up, don't fail.
        self.chan.try_send(DropIfRefMessage);
    }
}
impl SessionIf {
    pub fn query_freshness_by_addr(&self,
                                   addr: Address) -> Future<bool> {
    }
    pub fn report_source_dep(&self,
                             addr: SourceAddress) {
        let dep = AddressDep(addr);
        self.chan.send(ReportDepMessage(self.addr.clone(), dep));
    }
}
