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

use std::cell::{Cell, RefCell};
use std::collections::HashMap;

use sqlite;
use sqlite::{SqliteResult, BindArg};
use super::supervisor::{supervisor};

local_data_key!(database_: Database)

pub struct Database {
    next_savepoint_id: Cell<uint>,
    db: RefCell<sqlite::Database>,
    prepared: RefCell<HashMap<&'static str, sqlite::Cursor<'static>>>,
}
impl Database {
    fn with_db<R>(&self, f: |&mut sqlite::Database| -> R) -> R {
        let borrow = self.db.borrow_mut();
        f(&mut *borrow)
    }

    pub fn savepoint(&self) -> Savepoint {
        let id = self.next_savepoint_id.get();
        let id_str = id.to_string();
        self.with_db(|db| {
            db.exec(format!("SAVEPOINT {}", id_str).as_slice())
                .ok().expect("SAVEPOINT failed");
        });
        self.next_savepoint_id.set(id + 1);
        Savepoint(id, id_str)
    }
    fn reset_to_savepoint(&self, sp: uint) {
        let sp_lvl = self.next_savepoint_id.get();
        if sp_lvl <= sp {
            fail!("savepoint `{}` already released or rolledback", sp);
        }
        self.next_savepoint_id.set(sp);
    }

    fn with_prepared<R>(&self, f: |&mut HashMap<&'static str, sqlite::Cursor<'static>>| -> R) -> R {
        let borrow = self.prepared.borrow_mut();
        f(&mut *borrow)
    }

    fn prep_cursor(&self, stmt: &'static str) -> sqlite::Cursor<'static> {
        use std::mem::transmute;
        self.with_db(|db| unsafe {
            let cursor = db.prepare(stmt, &None)
                .ok().expect("couldn't prepare statement");
            transmute(cursor)
        })
    }

    pub fn prepare<R>(&self,
                      stmt: &'static str,
                      f: |&mut sqlite::Cursor| -> R) -> R {
        use std::mem::transmute;

        self.with_prepared(|prepared| {
            let cursor = prepared.find_or_insert_with
                (stmt,
                 |_| self.prep_cursor(stmt) );
            let result = f(cursor);
            cursor.clear_bindings();
            result
        })
    }
    pub fn select<R>(&self,
                     stmt: &'static str,
                     args: &[BindArg],
                     f: |&mut sqlite::Cursor| -> R) -> Vec<R> {
        self.prepare(stmt,
                     |c| {
                         assert_eq!(c.bind_params(args),
                                    sqlite::SQLITE_OK);
                         let mut dest: Vec<R> = Vec::new();
                         loop {
                             let code = c.step();
                             if code == sqlite::SQLITE_DONE {
                                 break;
                             }
                             assert_eq!(code, sqlite::SQLITE_ROW);
                             dest.push(f(c));
                         }
                         dest
                     })
    }

    pub fn insert(&self,
                  stmt: &'static str,
                  args: &[BindArg]) -> u64 {
        self.with_prepared(|p| {
            let cursor = p.find_or_insert_with
                (stmt,
                 |_| self.prep_cursor(stmt) );
            assert_eq!(cursor.bind_params(args), sqlite::SQLITE_OK);
            assert_eq!(cursor.step(), sqlite::SQLITE_OK);
            assert_eq!(cursor.clear_bindings(), sqlite::SQLITE_OK);
        });
        self.with_db(|db| db.get_last_insert_rowid() as u64 )
    }
    /// Returns the number of rows affected.
    pub fn update(&self,
                  stmt: &'static str,
                  args: &[BindArg]) -> u64 {
        self.with_prepared(|p| {
            let cursor = p.find_or_insert_with
                (stmt,
                 |_| self.prep_cursor(stmt) );
            assert_eq!(cursor.bind_params(args), sqlite::SQLITE_DONE);
            assert_eq!(cursor.clear_bindings(), sqlite::SQLITE_OK);
        });
        self.with_db(|db| db.get_changes() as u64 )
    }
}
impl Drop for Database {
    fn drop(&mut self) {
        let &Database {
            db: db,
            prepared: stmts,
            next_savepoint_id: sp_lvl,
        } = self;
        assert_eq!(sp_lvl.get(), 0);
        // we MUST drop prepared statements before disconnecting the database.
        drop(stmts);
        drop(db);
    }
}

pub struct Savepoint(uint, String);
impl Savepoint {
    fn str<'a>(&'a self) -> &'a str {
        let &Savepoint(_, ref inner) = self;
        inner.as_slice()
    }
    fn id(&self) -> uint {
        let &Savepoint(id, _) = self;
        id
    }
    fn internal_release(db: &Database, id: uint, name: &str) -> SqliteResult<bool> {
        let result = db.with_db(|db| db.exec(format!("RELEASE {}", name).as_slice()) );
        db.reset_to_savepoint(id);
        result
    }
    fn internal_rollback(db: &Database, id: uint, name: &str) -> SqliteResult<bool> {
        let result = db.with_db(|db| db.exec(format!("ROLLBACK TO {}", name).as_slice()) );
        db.reset_to_savepoint(id);
        result
    }
    pub fn release(self) -> SqliteResult<bool> {
        let Savepoint(id, id_str) = self;
        Savepoint::internal_release(database(), id, id_str.as_slice())
    }
    pub fn rollback(self) -> SqliteResult<bool> {
        let Savepoint(id, id_str) = self;
        Savepoint::internal_rollback(database(), id, id_str.as_slice())
    }
}
impl Drop for Savepoint {
    fn drop(&mut self) {
        use std::task::failing;
        let db = database();
        if db.next_savepoint_id.get() > self.id() {
            let _ = if failing() {
                Savepoint::internal_rollback(db, self.id(), self.str())
            } else {
                Savepoint::internal_release(db, self.id(), self.str())
            };
        }
    }
}

pub fn database() -> &'static Database {
    use std::mem::transmute;
    if database_.get().is_none() {
        let db_path = supervisor().get_database_path();
        let db_path_str = db_path.display().to_string().as_slice();
        match sqlite::open(db_path_str) {
            Ok(db) => {
                let db = Database {
                    next_savepoint_id: Cell::new(0),
                    db: RefCell::new(db),
                    prepared: RefCell::new(HashMap::new()),
                };
                database_.replace(Some(db));
            }
            Err(code) => {
                fail!("opening database failed: `{}`", code);
            }
        }
    }
    unsafe { transmute(&*database_.get().unwrap()) }
}

#[cfg(test)]
pub fn set_testing_db(db: sqlite::Database) {
    let db = Database {
        next_savepoint_id: Cell::new(0),
        db: RefCell::new(db),
        prepared: RefCell::new(HashMap::new()),
    };
    database_.replace(Some(db));
}

pub type EnumVariantKey = u32;
pub trait EnumVariantToKey<T = EnumVariantKey> {
    fn enum_variant_key(&self) -> Option<T>;
}

pub trait RocketDb {

}

pub trait ToBindArg {
    fn to_bind_arg(&self) -> sqlite::BindArg;
}

macro_rules! impl_to_bind_arg(
    (for [$($ty:ty),*] => $expr:expr) => {
        $(
            impl ToBindArg for $ty {
                fn to_bind_arg(&self) -> sqlite::BindArg {
                    return $expr;
                }
            }
            impl ToBindArg for Option<$ty> {
                fn to_bind_arg(&self) -> sqlite::BindArg {
                    self.map(|v| v.to_bind_arg() )
                        .unwrap_or(sqlite::Null)
                }
            }
         )*
    })

// IDK.
/*impl_to_bind_arg!(for [i8, u8, i16, u16, i32, u32] =>
                  {
                      let v = *self as int;
                      sqlite::Integer(v)
                  })
impl_to_bind_arg!(for [i64, u64] => sqlite::Integer64(*self as i64))
impl_to_bind_arg!(for [f32, f64] => sqlite::Float64(*self as f64))
impl_to_bind_arg!(for [String]   => sqlite::Text(self.clone()))*/

impl<T: ToBindArg> ToBindArg for Option<T> {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        self.map(|v| v.to_bind_arg()).unwrap_or(sqlite::Null)
    }
}

impl ToBindArg for i8 {
        fn to_bind_arg(&self) -> sqlite::BindArg {
            (sqlite::Integer(*self as int))
        }
    }
impl ToBindArg for u8 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        (sqlite::Integer(*self as int))
    }
}
impl ToBindArg for i16 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        (sqlite::Integer(*self as int))
    }
}
impl ToBindArg for u16 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        (sqlite::Integer(*self as int))
    }
}
impl ToBindArg for i32 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        (sqlite::Integer(*self as int))
    }
}
impl ToBindArg for u32 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        (sqlite::Integer(*self as int))
    }
}
impl ToBindArg for i64 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        sqlite::Integer64(*self as i64)
    }
}
impl ToBindArg for int {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        sqlite::Integer64(*self as i64)
    }
}
impl ToBindArg for uint {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        sqlite::Integer64(*self as i64)
    }
}
impl ToBindArg for u64 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        sqlite::Integer64(*self as i64)
    }
}
impl ToBindArg for f32 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        sqlite::Float64(*self as f64)
    }
}
impl ToBindArg for f64 {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        sqlite::Float64(*self as f64)
    }
}
impl ToBindArg for String {
    fn to_bind_arg(&self) -> sqlite::BindArg {
        sqlite::Text(self.clone())
    }
}
