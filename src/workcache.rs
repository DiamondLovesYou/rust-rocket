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

use std::comm::{Port, Chan};
use std::io;
use std::io::{PortReader, ChanWriter, IoResult};
use std::hash::sip::SipState;

use address::SourceAddress;
use override::Overrides;

/// singleton task POV
enum Message {
    AddIfRefMsg,
    /// if Some(..), send back the post de-reference ref count.
    /// used during shutdown to assert there are no more refs.
    DropIfRefMsg(Option<Chan<u64>>),
    
    /// sends back true if the work was identical to existing work.
    CacheMsg(Chan<CachingResult>, Port<Vec<u8>>, Key),
    
    HelperFinishedCachingMsg(HelperCachingResult),
}

pub type CachingResult = IoResult<(KeyId, bool)>;
struct HelperCachingResult {
    hif: HelperIf,
    result: IoResult<(KeyId, Entry)>,
    ret: Chan<CachingResult>,
}

pub trait CachableWork: Encodable + Decodable + Hash {}

#[deriving(Encodable, Decodable, Hash, Clone, Eq)]
pub struct Key(SourceAddress, Overrides);

/// a more light-weight key into the cache.
#[deriving(Encodable, Decodable, Hash, Clone, Eq)]
pub struct KeyId(u64);
impl KeyId {
    fn unwrap(&self) -> u64 {
        let &KeyId(id) = self;
        id
    }
}

#[deriving(Encodable, Decodable, Clone, Eq)]
struct Entry {
    Key:  Key,
    tid:  TargetId,
    path: Path,
    hash: u64,
}
static CACHE_ARTIFACT_SUBPATH: &'static str = ".cache";
/// Make sure 
static DB_VERSION: u64 = 0;
#[deriving(Encodable, Decodable)]
struct Db {
    /// we need consistent hashes:
    hash_keys: (u64, u64),
    cache: HashMap<KeyId, Entry>,
    
    cache_path: Path,
}
/// houses the runtime needs of a workcache task. It is not saved.
struct Session {
    db: Db,
    db_file: File,
    hasher: SipState,
    own: SessionIf,
    ref_count: u64,
    /// the helpers waiting for work.
    helpers: RingBuf<HelperIf>,
    /// the queue of work waiting for helpers.
    work:    RingBuf<HelperWork>,
}
impl Session {
    fn new((port, chan): (Port<Message>, Chan<Message>),
           wc_db_path: &Path, artifact_path: &Path) -> IoResult<SessionIf> {
        let mut file = try!(File::open_mode(wc_db_path, Open, ReadWrite));

        fn load_db_or_default(file: &mut File, artifact_path: &Path) -> Db {
            fn init_default(artifact_path: &Path) -> Db {
                Db {
                    hash_keys: (rand::random(), rand::random()),
                    cache: HashMap::new(),
                    keys: HashMap::new(),
                    artifact_path: artifact_path.join(CACHE_ARTIFACT_SUBPATH),
                }
            }

            let json = match json::from_reader(&mut file as &mut Reader) {
                Ok(j) => j.unwrap(),
                e => {
                    return init_default(artifact_path);
                }
            };
            let mut decoder = json::Decoder::new(json);
            let version = decoder.read_u64();
            if version != DB_VERSION {
                init_default(artifact_path)
            } else {
                Decodable::decode(&mut decoder)
            }
        }
        let own = SessionIf {
            chan: chan,
        };
        spawn(proc() {
                let db = load_db_or_default(&mut file, artifact_path);
                let (k0, k1) = db.hash_keys.clone();
                let mut sess = Session {
                    hasher: SipState::new_with_keys(k0, k1),
                    db: db,
                    db_file: file,
                    own: own.clone(),
                    ref_count: 0u,
                    helpers: RingBuf::new(),
                    work:    RingBuf::new(),
                };
                sess.task(port)
            });
        own
    }

    fn send_helper_work(&self,
                        hif:  &HelperIf,
                        work: HelperWork) {
        hif.send(HelperWork {
                own: Some(hif.clone()),
                .. work
            });
    }
    fn hash_key(&self,
                k: &Key) -> KeyId {
        k.hash(&mut self.hasher);
        self.hasher.reset();
    }
    fn record_hash_key(&mut self,
                       k: &Key) -> KeyId {
        let kid = self.hash_key(k);
        self.db.keys.mangle(kid,
                            (),
                            |_, _| k.clone(),
                            |_, _, _| ());
        kid
    }


    fn hdl_cache_msg(&mut self,
                     client_ret: Chan<CachingResult>,
                     incoming: Port<~[u8]>,
                     key: Key) {
        let kid = self.record_hash_key(key);
        let work = HelperWork {
            key: kid,
            ret: self.own.clone(),
            client_ret: client_ret,
            path: self.db.cache_path.join(Path::new(format!("{:}", kid.unwrap()))),
            prev_entry: None,
            recv: incoming,
            ret: client_ret,
            own: None,
        };
        match self.helpers.pop_front() {
            Some(hif) => {
                self.send_helper_work(hif, work);
            }
            None => {
                self.work.push_back(work);
            }
        }
    }
    fn hdl_helper_finished_msg(&mut self,
                               result: HelperCachingResult) {
        let result = match self.work.pop_front() {
            None => {
                self.helpers.push_back(result.hif);
                result.result
            },
            Some(work) => {
                // skip adding to the helpers queue only to remove
                // it sometime later in this function.
                self.send_helper_work(result.hif, work);
                result.result
            }
        };

        // FIXME(rdiamond): report errors.
        let (kid, entry) = match result {
            Err(e) => fail!(e.to_str()),
            Ok(ok) => ok,
        };

        // check to see if any body else has finished first.
        
    }

    fn process_messages<T: Iterator<Message>>(&mut self, msgs: T) {
        for msg in msgs {
            match msg {
                DropIfRefMsg(None) => { self.ref_count -= 1; },
                DropIfRefMsg(Some(ret)) => {
                    self.ref_count -= 1;
                    ret.try_send(self.ref_count);
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

    fn task(&mut self, port: Port<Message>) {
        /// FIXME: tune this.
        static HELPER_COUNT: uint = std::rt::default_sched_threads();

        for _ in iter::range(0, HELPER_COUNT) {
            let hif = HelperSession::new(Chan::new(), self.db.hash_keys.clone());
            self.helpers.push_back(hif);
        }

        self.process_msgs(port.iter());
    }
}
struct HelperSession {
    port: Port<HelperWork>,
    hasher: SipState,
}
impl HelperSession {
    fn task(mut self) {
        for work in self.port.iter() {
            work.do_work(&mut self);
        }
    }

    fn new((port, chan): (Port<HelperWork>, Chan<HelperWork>),
           (key0, key1): (u64, u64)) -> HelperIf {
        spawn(proc() {
                let mut sess = HelperSession {
                    port: port,
                    hasher: SipState::new_with_keys(key0, key1),
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
    ret: Chan<Message>,
    client_ret: Chan<CachingResult>,
    path: Path,
    prev_entry: Option<Entry>,
    recv: Port<Vec<u8>>,

    /// when in-queue, this is None. At the conclusion of work,
    /// the helper sends the interface back with the result.
    own: Option<HelperIf>,
}
impl HelperWork {
    /// Predicate: output file is closed
    fn ret(&self, sess: &mut HelperSession, err: Option<IoError>) {
        match err {
            Some(e) => {
                self.client_ret.try_send(Err(e.clone()));
                self.ret.try_send(HelperFinishedCachingMsg(Err(e)));
            }
            None => {
                let prev = self.prev_entry.unwrap_or_else(|| Entry {
                        hash: sess.hasher.result(),
                        path: self.path.clone(),
                        });
                let (entry, is_same) = if sess.hasher.result() == prev.hash {
                    (prev, true)
                } else {
                    (Entry {
                            hash: sess.hasher.result(),
                            path: self.path.clone(),
                    }, false)
                }
                self.ret.send(HelperFinishedCachingMsg(Ok((self.key, entry))));
            }
        }
        sess.hasher.reset();
    }
    fn do_work(&self, sess: &mut HelperSession) {
        {
            let fout = match File::open_mode(self.path, Open, Write) {
                Ok(fout) => fout,
                e => {
                    self.ret(sess, Some(e))
                        return;
                }
            };
            let mut size: i64 = 0;
            for buf in self.work.recv.iter() {
                match fout.write(buf) {
                    Ok(()) => (),
                    e => {
                        self.ret(sess, Some(e));
                        return;
                    }
                }
                match sess.hasher.write(buf) {
                    Ok(()) => (),
                    e => {
                        self.ret(sess, Some(e));
                        return;
                    }
                }
                size += buf.len();
            }
            match fout.truncate(size) {
                Ok(()) => (),
                e => {
                    self.ret(sess, Some(e));
                    return;
                }
            }
            match fout.datasync() {
                Ok(()) => (),
                e => {
                    self.ret(sess, Some(e));
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
    chan: Chan<HelperWork>,
}
impl HelperIf {
    
}

#[deriving(Clone)]
pub struct SessionIf {
    /// the chan to the global workcache task
    chan: Chan<Message>,
}
impl SessionIf {
    
    /// starts the global workcache task and returns the seeding interface
    pub fn new(db: &Path,
               artifact: &Path) -> IoResult<SessionIf> {
        Session::new(Chan::new(), db, artifact)
    }

    pub fn cache_cargo<T: Encodable>(&self,
                                     address: address::SourceAddress,
                                     overrides: override::Overrides,
                                     cargo: T) -> Future<CachingResult> {
        static SEND_BUFFER_BYTES: uint = 1024;
        let key = Key(address, overrides);
        let (port, chan) = Chan::new();
        let (ret, ret_chan) = Chan::new();
        self.chan.send(CacheMsg(ret, port, key));
        
        let mut writer = io::BufferedWriter::with_capacity(SEND_BUFFER_BYTES,
                                                           io::ChanWriter::new(chan));
        {
            let mut encoder = json::Encoder::new(&mut writer as &mut Writer);
            cargo.encode(&mut encoder);
        }
        writer.flush();
        Future::from_port(ret)
    }
                      
}
impl clone::Clone for SessionIf {
    fn clone(&self) -> SessionIf {
        self.chan.send(AddIfRefMsg);
        SessionIf {
            chan: self.chan.clone();
        }
    }
}
impl drop::Drop for SessionIf {
    fn drop(&mut self) {
        self.chan.send(DropIfRefMsg(None));
    }
}
