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

use getopts::{optopt, optmulti, optflag, optflagopt};
use getopts;
use rustc;
use std::os;

pub fn opt() {}


pub fn emulate_rustc(args: ~[~str]) {
    rustc::main_args(args);
}

#[main]
pub fn main() {

    let original_args = os::args();
    let mut args = original_args.clone();
    args.shift();

    let rustc_emulation_opt = optflag("c",
                                      "",
                                      "Emulate rustc (see rustb -c --help for more info)");
    let help_opt = optflag("h",
                           "help",
                           "This help message");

    let opts: [getopts::OptGroup, ..2] = [rustc_emulation_opt, help_opt];
    let matches = if_ok!(getopts(args, opts));
    if matches.opt_present("c") {
        emulate_rustc(original_args.iter().filter(|x| x != "-c" ).collect());
        return;
    } else if matches.opt_present("h") && matches.free.empty() || args.empty() {
        print_usage(original_args);
        return;
    }

    // quick check if we're in a build dir
    if os::getcwd().is_build_folder() {
        
    } else {
        
    }

}

pub trait BuildDir {
    fn is_build_folder(&self) -> bool;
}
impl BuildDir for path::Path {
    fn is_build_folder(&self) -> bool {
        self.join(RUSTB_ARTIFACT_PATH).exists()
    }
}

struct SessionData {
}
#[deriving(Clone)]
pub struct SessionIF {
    
}

pub fn top_level_opts() -> ~[getopts::OptGroup] {
    ~[optflag("h", "help", "This help message"),
      optflag("c", "", "Emulate rustc (see rustb -c -h for more info)"),

      /// All flags from now on are only designed for developers/builders (people, specifically).
      optflag("build", "", "Build all of the crates in a source dir OR \
              build a configured dir. ")]
}

pub fn print_usage(_cli_name: &str) {
    println!("FIXME");
}

pub fn process_commands(args: &mut [~str], processor: fn(~str, &mut [~str])) {
    loop {
        let top = match args.shift() {
            None => { break; }
            Some(t) => t,
        };
        processor(top, args);
    }
}
pub fn process_build_dir_command(cmd: ~str, args: &mut [~str]) {
    if cmd == "clean" {
    } else if cmd == "build" {
        
    } else {
        fail!("unknown build dir command");
    }
}
pub fn process_source_dir_command(cmd: ~str, args: &mut [~str]) {
    if cmd == "configure" {
        let src_path = os::get_cwd();
        let build_path = args.shift().map_or(src_path + "build", |path| Path::new(path) );
        
    }
}

fn push_opt_flags(push: |OptGroup|,
                  flag_fmt: |&'static str| -> ~str,
                  desc_fmt: |bool| -> ~str,
                  default: bool) {
    push(optflag("",
                 flag_fmt(select_word(!default, 0)),
                 desc_fmt(!default)));
    push(optflag("",
                 flag_fmt(select_word(default, 0)),
                 desc_fmt(default)));
}

static BOOL_WORDS_COUNT: uint = 3;
static BOOL_WORDS: &'static [&'static [&'static str, ..BOOL_WORDS_COUNT], ..2] = [
    ["enable", "with", "enabled"],
    ["disable", "without", "disabled"],
];

fn select_word(flagness: bool, word: uint) -> &'static str {
    match flagness {
        true => BOOL_WORDS[0],
        false => BOOL_WORDS[1],
    }[word]
}

pub struct OptDirectory {
    tag: &'static str,
    
}
pub struct Opt {
    tag: &'static
}

#[cfg(target_os = "freebsd")]
static CLANG_IS_DEFAULT: bool = true;
#[cfg(not(target_os = "freebsd"))]
static CLANG_IS_DEFAULT: bool = false;

fn push_cc_opts(push: |OptGroup|) {
    push_opt_flags(push,
                   |x| format!("{}-cxx-ccache", x),
                   |x| format!("For C++ and C dependancies, build {} ccache (default {})",
                               select_word(x, 1), select_word(!x, 2)),
                   false);

    push_opt_flags(push,
                   |x| format!("{}-cxx-clang", x),
                   |x| format!("For C++ and C dependancies, build {} clang instead of gcc \
                               (default is to use gcc, except on FreeBSD)",
                               select_word(x, 1)),
                   CLANG_IS_DEFAULT);
    
}
fn add_rust_cfg_opts(push: |OptGroup|) {
}
fn push_codegen_opts(push: |OptGroup|) {
    push_opt_flags(push,
                   |x| format!("{}-optimize", x),
                   |x| format!("Build {} optimizations (default {})",
                               select_word(x, 1), select_word(!x, 2)),
                   true);

    push_opt_flags(push,
                   |x| format!("{}-assertions", x),
                   |x| format!("Build {} assertions (default {})",
                               select_word(x, 1), select_word(!x, 2)),
                   true);
    push_opt_flags(push,
                   |x| format!("{}-landing-pads", x),
                   |x| format!("Build {} landing pads; used for unwinding (default {})",
                               select_word(x, 1), select_word(!x, 2)),
                   true);
    push_opt_flags(push,
                   |x| format!("{}-gc", x),
                   |x| format!("Garbage collect shared data (experimental) (default {})",
                               select_word(x, 1)),
                   false);
}
