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

// A supervisor is a task that holds the build instance and is responsible for
// the various manager tasks used. This involves restarting manager tasks if
// they fail.

use std::hash::sip::{SipHasher, SipState};

use sqlite;
use sqlite::Database;

use Build;
use workcache;
use super::{DiagIf, WorkcacheIf};

enum Message {
    GetDatabasePathMsg(Sender<Path>),
    GetBuildPathMsg(Sender<Path>),

    GetStableHasherMsg(Sender<SipState>),

    // used only during supervisor bootstrapping
    GetDiagIfMsg(Sender<DiagIf>),
}

struct Supervisor {
    recv:  Receiver<Message>,
    own:   SupervisorIf,
    build: Build,

    diag:  DiagIf,
    wc:    WorkcacheIf,
}
impl Supervisor {
    fn new(build: Build) -> SupervisorIf {
        let (s, r) = channel();
        let iface = SupervisorIf {
            sender: s,
        };
        spawn(proc() {
            let supervisor = Supervisor {
                recv:  r,
                own:   iface.clone(),
                build: build,

                diag:  super::diagnostics::SessionIf::new(),
                wc:    workcache::new_interface(iface.clone()),
            };
        });
        iface
    }
    fn task(&mut self) {
        for msg in self.recv.iter() {
            match msg {
                GetDatabasePathMsg(ret) => {
                    let _ = ret.send_opt(self.build.db_path());
                }
                GetStableHasherMsg(ret) => {
                    let _ = ret.send_opt(self.build.stable_hasher());
                }
                GetDiagIfMsg(ret) => {
                    let _ = ret.send_opt(self.diag.clone());
                }
            }
        }
    }
}

local_data_key!(supervisor_: SupervisorIf)

pub fn supervisor() -> &'static SupervisorIf {
    use std::mem::transmute;
    unsafe {
        transmute(&*supervisor_.get()
                  .expect("task is missing its supervisor"))
    }
}

#[deriving(Clone)]
pub struct SupervisorIf {
    sender: Sender<Message>,
}
impl SupervisorIf {
    pub fn bootstrap_task(&self) {
        let (s, r) = channel();
        self.sender.send(GetDiagIfMsg(s));
        supervisor_.replace(Some(self.clone()));
        super::initialize_diag(r.recv());
    }

    pub fn get_database_path(&self) -> Path {
        let (s, r) = channel();
        self.sender.send(GetDatabasePathMsg(s));
        r.recv()
    }

    pub fn get_build_path(&self) -> Path {
        let (s, r) = channel();
        self.sender.send(GetBuildPathMsg(s));
        r.recv()
    }
    pub fn stable_hasher(&self) -> SipState {
        let (s, r) = channel();
        self.sender.send(GetStableHasherMsg(s));
        r.recv()
    }
}

pub fn new_supervisor(build: Build) -> SupervisorIf {
    Supervisor::new(build)
}
