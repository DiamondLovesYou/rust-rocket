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

/*!

TODO designing this to:

1. facilitate embedding in IDEs, cmds, CI bots, etc,
2. connect rustc's diagnostic API to Rocket's, and
3. operate efficiently,

is turning out to be a prickly problem.

 */

use syntax::codemap::{BytePos, FileName, CodeMap, Span, mk_sp};
use syntax::diagnostic::EmitterWriter;
use std::cell::Cell;
use std::comm::{Sender, Receiver};
use std::collections::HashMap;
use std::default::Default;
use std::io::IoResult;
use std::str;
use std::sync::{Arc, mpsc_queue};

pub use rustc::driver::diagnostic::{Level, FatalError};
use rustc::driver::diagnostic::{Bug, Fatal, Error, Warning, Note,
                                RenderSpan, Emitter};

use address::Address;
use override::{Origin, SpanOrigin};
use super::address;

pub trait Driver {
    fn fatal<M: Str>(&self, origin: Origin, msg: M) -> !;
    fn err<M: Str>(&self, origin: Origin, msg: M);
    fn warn<M: Str>(&self, origin: Origin, msg: M);
    fn info<M: Str>(&self, origin: Origin, msg: M);
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

// TODO: This needs to be able send the various diags to their appropriate
// dest. For example, in terminal mode, messages are just sent to stdout,
// however in IDE mode messages should be sent to the IDE. Additionally for IDE,
// we will need to be able to handle managing multiple builds.
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
                    let c = self.errors
                        .find(&addr)
                        .map(|&c| c )
                        .unwrap_or(0);
                    ret.send(c);
                }
            }
        }
    }
}

#[deriving(Clone, Eq, PartialEq)]
pub enum NestedOrigin {
    RegEntry(Origin),
    NestedEntry(Option<(Address, BytePos, BytePos)>, Option<Box<NestedOrigin>>),
}

impl NestedOrigin {
    pub fn new(origin: Origin, cm: &CodeMap) -> NestedOrigin {
        match origin {
            SpanOrigin(mut sp) => {
                let filemap = cm.lookup_byte_offset(sp.lo).fm;
                let addr = address()
                    .clone()
                    .with_source(Path::new(filemap.name));
                unimplemented!();
                let file_start = filemap.start_pos;
                let new_sp = mk_sp(sp.lo - file_start,
                                   sp.hi - file_start);
                let nested = sp.expn_info
                    .map(|expn_info| {
                        box NestedOrigin::new(SpanOrigin(expn_info.call_site),
                                              cm)
                    });
                NestedEntry(Some((addr, new_sp.lo, new_sp.hi)), nested)
            }
            _ => NestedEntry(None, None),
        }
    }
}

#[deriving(Clone)]
pub struct Emittion {
    addr: Address,

    // Note to Rocket devs: if SpanOrigin, the Span offsets must be normalized
    // as if the offending file is the only member of the CodeMap. This is so we
    // don't have to share or otherwise copy CodeMaps.
    origin:  NestedOrigin,
    original_origin: Origin,
    //lvl:     Level,
}

type DiagQueue = Arc<mpsc_queue::Queue<Emittion>>;

#[deriving(Clone)]
pub struct SessionIf {
    errors: Cell<uint>,
    queue: DiagQueue,
}

impl SessionIf {
    pub fn new() -> SessionIf {
        SessionIf {
            errors: Cell::new(0),
            queue:  Arc::new(mpsc_queue::Queue::new()),
        }
    }
    // An expected failure.
    pub fn fail(&self) -> ! {
        fail!(FatalError)
    }
    fn bump_error_count(&self) {
        self.errors.set(self.errors.get() + 1);
    }
    pub fn errors(&self) -> uint {
        self.errors.get()
    }
}
impl Driver for SessionIf {
    fn fatal<M: Str>(&self, origin: Origin, msg: M) -> ! {
        self.bump_error_count();
        let addr = address().clone();

        self.fail();
    }
    fn err<M: Str>(&self, origin: Origin, msg: M) {
    }
    fn warn<M: Str>(&self, origin: Origin, msg: M) {
    }
    fn info<M: Str>(&self, origin: Origin, msg: M) {

    }
    pub fn fail_if_errors(&self) {
        if self.errors() != 0 {
            self.fail();
        }
    }
}
impl Emitter for SessionIf {
    fn emit(&mut self, cmsp: Option<(&CodeMap, Span)>,
            msg: &str, code: Option<&str>, lvl: Level) {
    }
    fn custom_emit(&mut self, cm: &CodeMap,
                   sp: RenderSpan, msg: &str, lvl: Level) {
    }
}
