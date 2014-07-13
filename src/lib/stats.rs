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

//! Stat collection, accounting, and consideration.
//! FIXME(diamond): collect memory consumption and hdd activity stats.

use std::comm::Sender;
use std::collections::HashMap;

use address::Address;

enum Message {
    
}

#[deriving(Clone, Encodable, Decodable)]
pub struct Router {
    /// Every train yard has its own entry here:
    time: Vec<u64>,
}

#[deriving(Encodable, Decodable)]
struct Session {
    times: HashMap<Address, u64>,
}
#[deriving(Clone)]
pub struct SessionIf {
    chan: Sender<Message>,
}
