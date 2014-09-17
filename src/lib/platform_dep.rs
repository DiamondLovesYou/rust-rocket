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

use std::path::Path;
use std::collections::HashMap;
use std::cell::RefCell;

use driver::diagnostics;
use driver::diagnostics::Driver;
use override::Origin;

#[deriving(Encodable, Decodable)]
pub struct Session {
    tools: HashMap<ToolId, Path>,

    libs: HashMap<String, Path>,
}
#[deriving(Hash, Encodable, Decodable, Clone, Eq, PartialEq)]
pub enum CCompilerType {
    GccCC,
    ClangCC,
}

#[deriving(Hash, Encodable, Decodable, Clone, Eq, PartialEq)]
pub enum ToolId {
    CCompilerTool(Option<CCompilerType>),
    CXXCompilerTool(Option<CCompilerType>),
    LDLinkerTool,
    ARArchiverTool,
    PythonTool(uint), // version 2 or 3.
    GdbTool,
    LldbTool,
    PerlTool,
    ValgrindTool,
    PerfTool,
    ISCCTool,   // I'm not sure what this is
    CMakeTool,
    GitTool,    // I intend to transition this to libgit, eventually.
    ZCatTool,
    CurlTool,   // I intend to transition this to libcurl, eventually.
    PaxCtlTool, // Linux only.
    NodeJsTool, // Ewww. Sadly, its currently needed for doc gen.
    LuaTexTool,
    XeTexTool,
    PdfLaTexTool,
    PanDocTool,
    AdbTool,

    // Platform tool X goes here; don't be shy.
    CustomTool(String),
}

impl ::FromStrWithOrigin for ToolId {
    fn from_str_with_origin(s: &str, origin: Origin) -> Option<ToolId> {
        Some(match s {
            // Prefer these:
            "cc"      => CCompilerTool(None),
            "c++"     => CXXCompilerTool(None),
            // to these:
            "clang"   => CCompilerTool(Some(ClangCC)),
            "clang++" => CXXCompilerTool(Some(ClangCC)),
            "gcc"     => CCompilerTool(Some(GccCC)),
            "g++"     => CXXCompilerTool(Some(GccCC)),

            "ld"      => LDLinkerTool,
            "ar"      => ARArchiverTool,

            "python"  => PythonTool(2),
            "python3" => PythonTool(3),

            "gdb"     => GdbTool,
            "lldb"    => LldbTool,

            "perl"    => PerlTool,
            // valgrind
            "perf"    => PerfTool,
            // ISCC
            "cmake"   => CMakeTool,
            "git"     => GitTool,
            // zcat
            "curl"    => CurlTool,
            "paxctl"  => PaxCtlTool,
            "nodejs"  => NodeJsTool,
            // LuaTex
            // XeTex
            // PdfLaTex
            // PanDoc
            "adb"     => AdbTool,

            _ => {
                diagnostics().warn(origin, "unknown tool");
                CustomTool(s.to_string())
            }
        })
    }
}

#[deriving(Encodable, Decodable)]
pub struct Tool {
    tool: ToolId,
    path: Path,
    /// lazily initialized
    version_str: RefCell<Option<String>>,
}

pub trait ToolInfo {
    fn tool(&self) -> ToolId;
    fn version_str(&self) -> String;
}



impl Session {
    fn new() {
    }
}
