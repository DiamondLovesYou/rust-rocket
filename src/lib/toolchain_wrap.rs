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

/// Stuff related to toolchain impersonation.

use std::path::Path;

#[deriving(Encodable, Decodable)]
pub enum Compiler {
    Clang(~str),
    Gcc(~str),
}
impl Compiler {
    pub fn get_cmd(&self) -> ~str {
        match self {
            &Clang(ref cmd) | &Gcc(ref cmd) => cmd.clone(),
        }
    }
}

#[deriving(Encodable, Decodable)]
pub struct State {
    c: Compiler,
    cxx: Compiler,
    ar: ~str,
    ld: ~str,
    
    // global here means on all files
    c_global_overrides: Option<~[Override<~str>]>,
    cxx_global_overrides: Option<~[Override<~str>]>,

    file_overrides: Option<HashMap<~str, Overrides<~str>>>,
}
impl State {
    pub fn build_new() -> Path {
    }
}
pub struct StateFile(Path);
impl StateFile {
    
}
impl Drop for StateFile {
    fn drop(&mut self) {
        fs::unlink(*self)
    }
}

pub enum Tool {
    CC,
    CXX,
    AR,
    LD,
}
impl FromStr for Tool {
    fn from_str(s: &str) -> Option<Tool> {
        match s {
            "cc" =>  Some(CC),
            "c++" => Some(CXX),
            "ar" =>  Some(AR),
            "ld" =>  Some(LD),
            _ =>     None,
        }
    }
}

pub struct Invocation<'a> {
    state_file: Path,
    print_invocation: bool,
    // are we under a configure script? if so we don't need to resolve addresses.
    configure: bool,
    tool: Tool,
    opts: &'a [~str],
}

impl<'a> Invocation<'a> {
    pub fn new(state: &str,
               print_invocation: bool,
               tool: &str,
               opts: &'a [String]) -> Invocation<'a> {
        Invocation {
            state_file: Path::new(state),
            print_invocation: print_invocation,
            tool: from_str(tool).expect("unknown tool specified; this is more than likely a bug"),
            opts: opts,
        }
    }

    pub fn run(&self) {
        // don't try block this; if we can't read the state file, we really do need to fail!().
        let state = {
            use serialize::ebml::reader::{Decoder, Doc};
            let state_bytes = try!({try!(File::open(self.state_file))}.read_to_end());
            let mut decoder = Decoder(Doc(state_bytes));
            decode(&mut decoder)
        };
        
        match self.tool {
            CC => {
            }
            CXX => {
            }
            AR => {
            }
            LD => {

            }
        }
    }
}
