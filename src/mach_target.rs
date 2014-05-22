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

use syntax::abi;
use rustc::driver::driver::{get_os, get_arch};
use std::to_str::ToStr;
use std::from_str::FromStr;
use std::option::{Option, Some};

pub struct Target {
    full: ~str,
    arch: abi::Architecture,
    os: Option<abi::Os>,
    manufacturer: Option<~str>,
    kernel: Option<~str>,
}

impl Target {
    pub fn new(full: ~str) -> Target {
        Target {
            arch: match get_arch(full) {
                Some(arch) => arch,
                None => early_error("unknown architecture: " + full)
            },
            os: get_os(full),
            manufacturer: None,
            kernel: None,
            full: full,
        }
    }
    pub fn is_unixy(&self) -> bool {
        self.os.map_or(false, |os| os != abi::OsWin32 )
    }
    pub fn is_windowsy(&self) -> bool {
        self.os.map_or(false, |os| os == abi::OsWin32 )
    }
    pub fn is_freestanding_or_unknown(&self) -> bool {
        self.os.is_none()
    }

}
impl fmt::Show for Target {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        f.pad(self.full)
    }
}
impl FromStr for Target {
    fn from_str(s: &str) -> Option<Target> {
        Some(Target::new(s.clone()))
    }
}

pub fn get_host_triple() -> Target {
    Target::new(rustc::driver::host_triple())
}
