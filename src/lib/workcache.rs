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

// TODO: sqlite.

use std;
use std::comm::{Receiver, Sender};
use std::{io, iter, default, rand, clone, ops};
use std::io::{ChanReader, ChanWriter, IoResult, IoError, Open, ReadWrite, Write};
use std::io::fs::File;
use std::hash::sip::SipState;
use std::hash::Hash;
use std::collections::{HashMap, RingBuf};
use std::sync::Future;
use serialize::{Encodable, Decodable};
use serialize::json;

use address::Address;
use driver;
use driver::WorkcacheIf;
use override::Overrides;
use TargetId;

pub use serialize::json::{Encoder, Decoder};

// singleton task POV
enum Message {
    AddIfRefMsg,
    // if Some(..), send back the post de-reference ref count.
    // used during shutdown to assert there are no more refs.
    DropIfRefMsg(Option<Sender<u64>>),

    // sends back true if the work was identical to existing work.
    CacheMsg(Sender<CachingResult>, Receiver<Vec<u8>>, Key),

    HelperFinishedCachingMsg(HelperCachingResult),
}

pub type CachingResult = IoResult<(KeyId, bool)>;
struct HelperCachingResult {
    hif: HelperIf,
    result: IoResult<(KeyId, Entry)>,
    ret: Sender<CachingResult>,
}

pub trait CachableWork<'a>: Encodable<Encoder<'a>, IoError> + Decodable<Decoder, IoError> {}

pub type Key = Address;

/// a more light-weight key into the cache.
#[deriving(Encodable, Decodable, Hash, Clone, Eq, PartialEq)]
pub struct KeyId(u64);
impl KeyId {
    fn unwrap(&self) -> u64 {
        let &KeyId(id) = self;
        id
    }
}

#[deriving(Clone)]
struct Entry {
    id: u64,
    Key:  Key,
    hash: u64,
    path: KeyId,
}

static CACHE_ARTIFACT_SUBPATH: &'static str = "cache";
// houses the runtime needs of a workcache task. It is not saved.
struct Session {
    cache_path: Path,
    hasher: SipState,
    own: SessionIf,
    ref_count: u64,
    // the helpers waiting for work.
    helpers: RingBuf<HelperIf>,
    // the queue of work waiting for helpers.
    work:    RingBuf<HelperWork>,
}
impl Session {
    fn new(sv: driver::SupervisorIf) -> SessionIf {
        let (s, r) = channel();
        let own = SessionIf {
            chan: s,
        };
        spawn(proc() {
                sv.bootstrap_task();
                let mut sess = Session {
                    cache_path: sv.get_build_path().join(CACHE_ARTIFACT_SUBPATH),
                    hasher: sv.stable_hasher(),
                    own: own.fake_clone(),
                    ref_count: 1u64,
                    helpers: RingBuf::new(),
                    work:    RingBuf::new(),
                };
                sess.task(r)
            });
        own
    }

    fn send_helper_work(&self,
                        hif:  &HelperIf,
                        work: HelperWork) {
        hif.chan.send(HelperWork {
                own: Some(hif.clone()),
                .. work
            });
    }
    fn hash_key(&self,
                k: &Key) -> KeyId {
        k.hash(&mut self.hasher);
        self.hasher.reset();
    }

    fn hdl_cache_msg(&mut self,
                     client_ret: Sender<CachingResult>,
                     incoming: Receiver<Vec<u8>>,
                     key: Key) {
        let kid = self.record_hash_key(&key);
        let work = HelperWork {
            key: kid,
            ret: self.own.chan.clone(),
            client_ret: client_ret,
            path: self.cache_path.join(Path::new(format!("{:}", kid.unwrap()))),
            prev_entry: None,
            recv: incoming,
            own: None,
        };
        match self.helpers.pop() {
            Some(hif) => {
                self.send_helper_work(&hif, work);
            }
            None => {
                self.work.push(work);
            }
        }
    }
    fn hdl_helper_finished_msg(&mut self,
                               result: HelperCachingResult) {
        let result = match self.work.pop() {
            None => {
                self.helpers.push(result.hif);
                result.result
            },
            Some(work) => {
                // skip adding to the helpers queue only to remove
                // it sometime later in this function.
                self.send_helper_work(&result.hif, work);
                result.result
            }
        };

        // FIXME(rdiamond): report errors.
        let (kid, entry) = match result {
            Err(e) => fail!("{}", e.to_string()),
            Ok(ok) => ok,
        };

        // check to see if any body else has finished first.

    }

    fn process_messages<T: Iterator<Message>>(&mut self, mut msgs: T) {
        for msg in msgs {
            match msg {
                DropIfRefMsg(None) => { self.ref_count -= 1; },
                DropIfRefMsg(Some(ret)) => {
                    self.ref_count -= 1;
                    ret.send_opt(self.ref_count);
                }
                AddIfRefMsg => { self.ref_count += 1; },

                CacheMsg(c, p, k) => self.hdl_cache_msg(c, p, k),
                HelperFinishedCachingMsg(result) => {
                    self.hdl_helper_finished_msg(result)
                }
            };

            if self.ref_count == 0 {
                break;
            }
        }
    }

    fn task(&mut self, port: Receiver<Message>) {
        /// FIXME: tune this.
        static HELPER_COUNT: uint = std::rt::default_sched_threads();

        for _ in iter::range(0, HELPER_COUNT) {
            let hif = HelperSession::new(channel(), self.hasher.clone());
            self.helpers.push(hif);
        }

        self.process_messages(port.iter());
    }
}
struct HelperSession {
    port: Receiver<HelperWork>,
    hasher: SipState,
}
impl HelperSession {
    fn task(mut self) {
        for mut work in self.port.iter() {
            work.do_work(&mut self);
        }
    }

    fn new((chan, port): (Sender<HelperWork>, Receiver<HelperWork>),
           hasher: SipState) -> HelperIf {
        spawn(proc() {
                let mut sess = HelperSession {
                    port: port,
                    hasher: hasher,
                };
                sess.task();
            });

        HelperIf {
            chan: chan,
        }
    }
}
struct HelperWork {
    key: KeyId,
    ret: Sender<Message>,
    client_ret: Sender<CachingResult>,
    path: Path,
    prev_entry: Option<Entry>,
    recv: Receiver<Vec<u8>>,

    /// when in-queue, this is None. At the conclusion of work,
    /// the helper sends the interface back with the result.
    own: Option<HelperIf>,
}
impl HelperWork {
    // Predicate: output file is closed
    fn ret(&mut self, sess: &mut HelperSession, err: Option<IoError>) {
        match err {
            Some(e) => {
                self.client_ret.send_opt(Err(e.clone()));
                let res = HelperCachingResult {
                    result: Err(e),
                    hif: self.own.take().unwrap(),
                    sender: self.client_ret.clone(),
                };
                self.ret.send_opt(HelperFinishedCachingMsg(res));
            }
            None => {
                let (entry, is_same) = if Some(sess.hasher.result()) == self.prev_entry.map(|p| p.hash ) {
                    (self.prev_entry.unwrap(), true)
                } else {
                    (Entry {
                            hash: sess.hasher.result(),
                            path: self.path.clone(),
                    }, false)
                };
                let res = HelperCachingResult {
                    result: Ok((self.key, entry)),
                    hif: self.own.take().unwrap(),
                    sender: self.client_ret.clone(),
                };
                self.ret.send(HelperFinishedCachingMsg(res));
            }
        }
        sess.hasher.reset();
    }
    fn do_work(&mut self, sess: &mut HelperSession) {
        {
            let fout = match File::open_mode(&self.path, Open, Write) {
                Ok(fout) => fout,
                e => {
                    self.ret(sess, e.err());
                    return;
                }
            };
            let mut size: i64 = 0;
            for buf in self.recv.iter() {
                match fout.write(buf.as_slice()) {
                    Ok(()) => (),
                    e => {
                        self.ret(sess, e.err());
                        return;
                    }
                }
                match sess.hasher.write(buf) {
                    Ok(()) => (),
                    e => {
                        self.ret(sess, e.err());
                        return;
                    }
                }
                size += buf.len() as i64;
            }
            match fout.truncate(size) {
                Ok(()) => (),
                e => {
                    self.ret(sess, e.err());
                    return;
                }
            }
            match fout.datasync() {
                Ok(()) => (),
                e => {
                    self.ret(sess, e.err());
                    return;
                }
            }
        }
        self.ret(sess, None);
    }
}
enum HelperFlags {
    HelperWorking,
    HelperWaiting,
}
impl default::Default for HelperFlags {
    fn default() -> HelperFlags {
        HelperWaiting
    }
}

/// sadly, this still needs Clone for the brief moments when
/// we are sending the IF through the IF its encapsulating.
#[deriving(Clone)]
struct HelperIf {
    chan: Sender<HelperWork>,
}
impl HelperIf {

}
pub struct SessionIf {
    /// the chan to the global workcache task
    chan: Sender<Message>,
}
impl SessionIf {
    /// Clones this interface without sending an add ref message.
    fn fake_clone(&self) -> SessionIf {
        SessionIf {
            chan: self.chan.clone(),
        }
    }

    pub fn cache_cargo<'a, T: CachableWork<'a>>(&self,
                                                address: Address,
                                                cargo: T) -> Future<CachingResult> {
        static SEND_BUFFER_BYTES: uint = 1024;
        let (s, r) = channel();
        let (ret_s, ret_r) = channel();
        self.chan.send(CacheMsg(ret_s, r, address));

        let mut writer = io::BufferedWriter::with_capacity(SEND_BUFFER_BYTES,
                                                           io::ChanWriter::new(s));
        {
            unimplemented!();
        }
        writer.flush();
        Future::from_receiver(ret_r)
    }
}
impl clone::Clone for SessionIf {
    fn clone(&self) -> SessionIf {
        self.chan.send(AddIfRefMsg);
        SessionIf {
            chan: self.chan.clone(),
        }
    }
}
impl ops::Drop for SessionIf {
    fn drop(&mut self) {
        self.chan.send(DropIfRefMsg(None));
    }
}

pub fn new_interface(sv: driver::SupervisorIf) -> WorkcacheIf {
    Session::new(sv)
}
