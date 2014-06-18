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

use toolchain::tool::{Tool, Compiler, Cc, Cxx, Ar, Ld};

pub struct State {
    
}

pub struct Invocation<'a> {
    state: Path,
    print_invocation: bool,
    // are we under a configure script? if so we don't need to resolve addresses.
    configure: bool,
    tool: Tool,
    opts: &'a [String],
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
        use std::io::fs::File;
        use serialize::ebml::reader::Decoder;
        use serialize::ebml::Doc;
        // don't try-block this; if we can't read the state file, we really do need to fail!().
        let state = {
            let state_bytes = try!({try!(File::open(self.state_file))}.read_to_end());
            let mut decoder = Decoder::new(Doc::new(state_bytes));
            decode(&mut decoder)
        };
        
        match self.tool {
            Cc => {
            }
            Cxx => {
            }
            Ar => {
            }
            Ld => {

            }
        }
    }
}
