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

//! This mod holds the code for the manager for the dynamic build crate instances.
use std::comm::{Receiver, Sender};

use driver::database::{database, ToBindArg};
use super::{BuildCrate, BuildCrateId,
            SymbolIndex, SymbolId, SymbolKind};
use super::{StaticStrKind, FnSymbolKind};

enum Message {
    QuitMsg,

    FindSymbol(Sender<*const ()>, BuildCrateId, SymbolIndex),

}

struct Session {
    recv:   Receiver<Message>,
    dylibs: Vec<(BuildCrateId, Option<BuildCrate>)>,
}

impl Session {
    fn load_build_crates(&mut self) {
        static QUERY: &'static str =
            "SELECT id FROM build_crates";
        let db = database();
        let mut bcs = db.select(QUERY,
                                [],
                                |c| {
                                    (BuildCrateId(c.get_i64(0) as u64),
                                     None)
                                });
        bcs.sort();
        self.dylibs = bcs;
    }

    fn task(&mut self) {
        for msg in self.recv.iter() {
            match msg {
                QuitMsg => { return; }
            }
        }
    }
}

pub struct SessionIf {
    sender: Sender<Message>,
}
