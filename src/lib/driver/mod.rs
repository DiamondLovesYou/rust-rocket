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

use syntax::codemap::CodeMap;

use address::Address;
use driver::diagnostics::Driver;

pub mod database;
pub mod diagnostics;
pub mod error_codes;
pub mod session;
pub mod supervisor;

pub type BuildId = uint;

pub type DiagIf = diagnostics::SessionIf;
//pub type DepIf  = super::dep::SessionIf;
pub type WorkcacheIf = super::workcache::SessionIf;
pub type SupervisorIf = supervisor::SupervisorIf;

local_data_key!(diagnostics_: DiagIf)

pub fn initialize_diag(diag: DiagIf) {
    diagnostics_.replace(Some(diag));
}
pub fn deinitialize_diag() -> Option<DiagIf> {
    diagnostics_.replace(None);
}
pub fn diagnostics() -> DiagIf {
    diagnostics_.get()
        .expect("task diagnostics uninitialized!")
        .clone()
}

// The thread local address of what we're working on.
// NOTE: it is expected that this is set checkpoint style, and is otherwise immutable.
local_data_key!(address_: Address)

pub fn address() -> &'static Address {
    use std::mem::transmute;
    unsafe {
        transmute(&*address_.get()
                  .expect("task work address uninitialized!"))
    }
}
pub fn mut_address<R>(f: |&mut Address| -> R) -> R {
    let mut addr = address().clone();
    let ret = f(&mut addr);
    address_.replace(Some(addr));
    ret
}

pub struct NewTaskState {
    diag: DiagIf,
}

impl NewTaskState {
    pub fn new() -> NewTaskState {
    }
}
