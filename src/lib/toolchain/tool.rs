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

#[deriving(Encodable, Decodable, Clone, Eq, PartialEq)]
pub enum Compiler {
    Clang,
    Gcc,
}

#[deriving(Encodable, Decodable, Clone, Eq, PartialEq)]
pub enum Tool {
    Cc(Compiler),
    Cxx(Compiler),
    Ar,
    Ld,
}
impl FromStr for Tool {
    fn from_str(s: &str) -> Option<Tool> {
        match s {
            "cc" =>  Some(Cc),
            "gcc" => Some(Cc),
            "clang" => Some(Cc),
            "c++" => Some(Cxx),
            "ar" =>  Some(Ar),
            "ld" =>  Some(Ld),
            _ =>     None,
        }
    }
}
