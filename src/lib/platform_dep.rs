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

#[deriving(Encodable, Decodable)]
pub struct Session {
    tools: HashMap<Tool_, Path>,

    libs: HashMap<~str, Path>,
}
#[deriving(Encodable, Decodable)]
pub enum CCompilerType {
    Gcc,
    Clang,
}
#[deriving(Hash, Encodable, Decodable)]
pub enum ToolId {
    CCompilerTool(CCompilerType),
    CXXCompilerTool(CCompilerType),
    LDLinkerTool,
    ARArchiverTool,
    PythonTool(uint), // version 2 or 3.
    GdbTool,
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
}

#[deriving(Encodable, Decodable)]
pub struct Tool {
    tool: ToolId,
    path: Path,
    /// lazily initialized
    version_str: RefCell<Option<~str>>,
}

pub trait ToolInfo {
    fn tool(&self) -> Tool_;
    fn version_str(&self) -> ~str;
}



impl Session {
    fn new() {
    }
}
