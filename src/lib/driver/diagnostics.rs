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

use syntax::diagnostic::EmitterWriter;
use std::comm::{Sender, Receiver};
use std::collections::HashMap;
use std::default::Default;
use std::io::IoResult;
use std::str;

use override::Origin;
use address::Address;

pub trait Driver {
    fn fatal(&self, origin: Origin, msg: &str) -> !;
    fn err(&self, origin: Origin, msg: &str);
    fn warn(&self, origin: Origin, msg: &str);
    fn info(&self, origin: Origin, msg: &str);
    fn fail_if_errors(&self);
}

enum Message {
    AddIfRef,
    DropIfRef(Option<Sender<uint>>),

    // the diag task will send when it has finished using the reference to the
    // codemap.
    DiagMessage(Address, String),
    QueryErrorCountMessage(Address, Sender<uint>),
}

// TODO: This needs to be able send the various diags to their apprioprate dest. For example, in terminal mode, messages are just sent to stdout, however in IDE mode messages should be sent to the IDE. Additionally for IDE, we will need to be able to handle managing multiple builds.
// To keep things initially simple, I'm just coding support for terminal mode.
struct Session {
    ref_count: uint,
    errors: HashMap<Address, uint>,
}
impl Session {
    fn task(&mut self, port: Receiver<Message>) {
        use syntax::diagnostic::{Error, Bug, Fatal,
                                 Warning, Note};
        'msg_loop: for msg in port.iter() {
            match msg {
                AddIfRef => {
                    self.ref_count += 1;
                }
                DropIfRef(ret) => {
                    self.ref_count -= 1;
                    match ret {
                        Some(ret) => ret.send(self.ref_count),
                        _ => (),
                    }
                    if self.ref_count == 0 {
                        break 'msg_loop;
                    }
                }
                DiagMessage(addr, s) => {
                    println!("{}", s);
                }
                QueryErrorCountMessage(addr, ret) => {
                    ret.send(self.errors.find(addr).unwrap_or(0));
                }
            }
        }
    }
}

#[deriving(Clone)]
pub struct SessionIf {
    chan: Sender<Message>,
    addr: Address,
}

impl SessionIf {
    pub fn new() -> SessionIf {
        let (port, chan) = channel();
        spawn(proc() {
            let mut sess = Session {
                ref_count: 0
            };
                sess.task(port)
        });
        SessionIf {
            chan: chan,
            addr: Default::default(),
        }
    }
    pub fn emitter(&self) -> EmitterWriter {
        EmitterWriter::new(box self.clone() as Box<Writer + Send>)
    }
    pub fn errors(&self) -> uint {
        let (port, chan) = channel();
        let msg = QueryErrorCountMessage(port);
        self.chan.send_opt(msg);
        chan.recv()
    }
    pub fn abort_if_errors(&self) {
        if self.errors() != 0 {
            fail!();
        }
    }
}
impl Writer for SessionIf {
    fn write(&mut self, buf: &[u8]) -> IoResult<()> {
        let s = unsafe { str::raw::from_utf8(buf) };
        let msg = DiagMessage(self.addr.clone(), s.to_string());
        self.chan.send_opt(msg);
        Ok(())
    }
}
