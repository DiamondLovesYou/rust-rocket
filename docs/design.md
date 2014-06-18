This proposal is still fairly WIP, so kindly bare with me. I realize it has some rough edges, and that in some areas it lacks edges altogether. Before I begin though, let me first point out that rustb, as proposed, is not a package manager of any sort. I am not trying to solve the problems of rustpkg/cargo

# Rust Rocket: The Rust Builder
## From 10,000ft:
#### Target Rust projects, not overall general use for other languages:
* Build .S/.c/.cc/.cpp objects for crate inclusion, utilizing the appropriate (cross) tooling (gcc/clang) (ie: rustllvm, or rt); and
* Generated rust modules (WebIDL in Servo, llvmdeps.rs, libc.c, cmathconsts.c, rust-bindgen-ed modules, etc).
* 'Native' dependencies (LLVM, libuv, compiler-rt), generally with both configuration && make steps.

#### Developer productivity:
* Rich CLI;
* In & out of source builds;
* Multiple targets per build;
* Utilize crate syntax trees to determine when rebuilds are really necessary;
* Don't require full lib (rlib, dylib, etc) for extern crates to start expensive steps like trans;
* Configuration overriding: add flags, replace flags, ban flags, ban crate types, emit llvm ir, emit asm, add cflags, etc. on a temp or perm basis;
* Generating docs as a standard part of any build;
* Work out of the box for simple crates, w/o a build script, including those depending slightly on .c/.cc/.cpp objects;
* Cross compilation aware (should strive to eliminate as much boilerplate build configuration Stuff as reasonably possible); and
* Speeeeeed.

#### Dependency tracking:
* Phase useage aware (ie syntax phase deps need to depend on the libsyntax rustb links with and not the target libsyntax, they also need to be built for the host system, not the target);
* Cfg file for native pkg -> rustb dep mappings;
* Builds are registered on a user level, for dep fulfillment.

#### Tests:
* Of course, good ol' unit testing/benching (#[test] && #[bench]);
* Crate tests: compile-fail, run-fail, run-pass, run-build, auxiliary, bench, custom tests types (these are good for frameworks, not just Rust proper); and
* External wrappers (looking at Android && PNaCl).

#### Build crates (build.rs in root of source dir):
* No explicit file management of the build dir (it's tacky); plus later on we could give Rustb plugins the power to dictate build dir layout;
* Rules expressed with objects;
* Rustb manages the relations to the FS;
* Subtargets for syntax phase deps (ie rustc depends on libstd);
* Custom subtargets, for running things like game asset optimizations, 
* Exposed to the crate build pipeline (PNaCl/Emscripten);
* Debugging;
* Ability to replace librustc/libsyntax/even the build script itself in rustb during build (needed by Rust proper);
* Ability to dictate extern crate fulfillment (ie snapshot rustb provides link phase extern crates) (verily, overridable by build configuration); and
* *Everything* should be overridable.

### Late Game Magic:

* In-crate macro registration, ie don't require a separate crate for its use;
* Rich plugin arch: ie CI integration, package managers, git/hg integration, etc;
* Rich C-API: for a builder GUI or IDE;
* Separate utility for querying info about build dirs, ie querying the full .so path for target in a build.
* Package repository;
* Installation;
* Watching; ie:
  * rustb forks (runtime will need work for this); on change immediately starts build;
  * Sometime later, dev runs ```rustb build```; and
  * Rustb displays build output.
* Change based test running;
* Fine-grained trans;
* Use libclang to build C/CXX deps (actually, this would be fairly easy to impl; the hardest part would come from being forced to design a solution to the Platform Problem, ie finding the platform libclang.so lib);
* CLI build script management; and
* rust-bindgen.

### Desired Build Dir (out-of-tree; located under build/ in-tree):
* artifacts/   ```artifacts of the build process, rustb caches, what-have-you.
(note rustb will share between builds as much as it safely can)```
* docs/
* extern/    ```in-tree deps```
* targets/
  * *target name*/  ```by default, target triple named; can be named anything.```
    * *subtarget, if created by build.rs; if not move subdirs one level up*
      * *native deps, ie configure &| make deps*
      * objs/ ```.o object files from .S, .c, .cc, .cpp source```
      * *crates*
      * *crates*.log      ```segregated build logs```
      * *LLVM IR, pretty printed source, temps, etc if requested*
      * tests/   ```possibly also under a subtarget, if a subtarget has registered individual tests.```
        * run-pass/
        * run-fail/
        * auxiliary/
        * bench/
        * run-build/
        * *custom test*/
  * *next target*/
* .rustb/      ```rustb build property registry```

## Milestones:

1. Build Rust proper && replace rustc usage in the wild. I'd like to tackle just building Rust proper for now; after that's working more or less like a charm, we can focus on a good solution for installations. In the mean time, we can place installation procedures in the build crate.
2. Build Servo

#### TODO Apropos to Milestone One:



#### TODO Apropos to Milestone Two:

## Implementation Thoughts

### Overridding/Config:
General semantics: addition, subtraction, and replacement.

#### Global (ie isn't just applicable to Rust or C/CXX codes):
Assertions on/off;
Toolchain, ie Gcc, Clang, android cross, pnacl cross, emscripten cross, mingw cross etc;
Debug infos;
Optimization on/off;
PaX patching;
rpath usage;
link args;

#### Rust specific:
Codegen opts not in global;
Lint;
-Z opts;
pretty printing;
including-std-version;
etc etc

#### C/CXX/Asm specific:
*any applicable CLI flag*


My current thoughts regarding form are that these flags would be most conveniently expressed declaratively: declared with a specialized attribute, rustb would collect all of the various flags together, then for every target. go over all flags, selecting the ones with truthy cfg attributes. The specialized attribute would optionally take a single param, in the form of an address. These flags would work like overrides, making it perfectly valid to declare a flag that rather than adding some config detail, removes or replaces the detail.

### Addressing:
The addressing model is basically a virtual filesystem hierarchy. For simple projects where there is only one subtarget, the model is fairly simple:
Given:

foo.rs:
```rust
#[crate_type = "bin"];
#[crate_type = "rlib"];
pub fn bar() {
  println!("zzz");
}
pub fn main() {
  bar()
}
```
The resultant directory would be:
* foo/        ```an override placed here will propagate to foo's two crate types.```
* foo-bin/    ```note the crate type used here;```
* foo-rlib/   ```and here.```

Note the hyphen is used because it can't appear in a Rust identifier, and hence can never clash with an external crate decl. This hyphen segregates the crate name with the declared crate types. Now, if one were to apply an override to foo/, that override would also apply to foo-bin/ && foo-rlib/. If, say, one wanted to apply an override to a single crate type, one would use either foo-bin/ or foo-rlib/ depending on what crate type needed the override.
Continuing with the example above, lets add another crate to the project and have that crate reference foo/:

bar.rs:
```rust
#[crate_type = "bin"];
extern crate foo;

pub fn main() {
  foo::bar()
}
```
Our project now has two files: foo.rs && bar.rs. The resultant directory would be thus:
* foo/
* foo-bin/
* foo-rlib/
* bar/ 

note the lack of a crate type; rustb merges bar-bin/ with bar/ because the bar crate only specified one crate type; there's no need to have two dirs that map to the same 'place'.
* bar/foo/ 

^ Does not refer to foo-rlib/; ie overrides applied here are contained to the foo rlib that bar links with. This'll likely get expensive for large crates, so at the very least there'll be an option to apply the override to the referral, if not make that the default altogether.

Now for a look at a bit more realistic but still contrived example:

bar.rs (in project A, in repo git://example.com/foo/bar.git):
```rust
#[crate_type = "dylib"];
#[crate_type = "rlib"];
pub fn foobar() -> ~str { ~"foobar!" }
```
foo.rs (in project B):
```rust
#[crate_type = "bin"];
#[crate_type = "dylib"];
extern crate bar = "git://example.com/foo/bar.git#bar";
extern crate barfoo;

pub fn run() {
  println!(bar::foobar())
}

pub fn main() {
  run();
  barfoo::run()
}
```
barfoo.rs (also in project B):
```rust
#[crate_type = "rlib"];
pub fn run() {
  println!("barfoo!")
}
```
In this example, if bar is built in-tree, the hierarchy look like:
* foo/
  * bar/           ```same behavior as above.```
  * bar-dylib/   ```won't exist if one is using a prebuilt copy.```
  * bar-rlib/     ```also won't exist if one is using a prebuilt copy.```
  * barfoo/      ```will always exist.```
* foo-dylib/
  * bar/           ```note that here bar/'s true path is bar-dylib/; given, of course, an in-tree build of bar.```

One thing I've left out is when one wants to apply an override to an entire project, which I'm thinking will be addressed as /.
Recursive overrides are default, but there'll be an option to turn that off per override.
When all is said and done, there shall be a cli opt (something like --print-dir) to print the complete hierarchy.

For builds with subtargets, the address can be optionally prefixed with the subtarget name followed by a single colon to apply the override to an address specific to a subtarget, otherwise the override will apply to all subtargets.

Additionally, all crates have a test suffix for the testing harness.

### Subtargets/Cross Compilation:
Explicit management of build->host->target subtargets in build scripts isn't a necessity, even in your typical three stage compiler bootstrapping process:
* 'Build' subtargets should really only exist for Rust itself, instigated by overridding the build crate's otherwise implicit dependence on the running rustb instance. Additionally, in the pursuit of completeness, the build crate embedded in the snapshot rustb will also override the source of the standard crates for syntax extension crates, allowing those standard crates to depend on them (the snapshot uses statically linked crates, so this might require some 'tweaking' in rustc::metadata && possibly additional contortions to work with the linker; honestly not sure off the top of my head).
* 'Host' subtargets are required if and only if as part of the build pipeline for a crate cited as a build output (including runtime deps) by the project at hand has:
  * A tool that needs to be built and run, and it's output used as an input to (this includes things like llvmdeps.rs which is generated from a script with input from a tool, llvm-config, which is built in a dependency, LLVM);
  * A build output depends on a crate sourced by the current project; or
  * A runtime dependency crate uses a syntax phase dependency sourced in-project.
* 'Target' subtargets are specified solely by configuration (ie, in the case of building Rust proper, a user specifying the desire to target 'le32-unknown-nacl'), and includes only runtime dependencies of outputs of the project and runtime dependency items of those outputs (example: stage2 libstd would be a runtime dependency of rustc, which in this context would be considered a host tool).

A more subtle point stemming from thee above rules is that a system wide, direct from build, installation (given an adequate installation procedures, of course) would only be applicable to host tools, just as only target tools would be interesting to a packager/installer generator. Additionally, questions are raised about how tests need to be run. For Rust proper, tests are run mostly against host tools, building the various run-pass, run-fail etc etc tests into collective binaries targeting the configured targets, running the resulting bins where it makes sense. This of course is a bit of kludge for more simple projects: present day nomenclature would dictate that builds *target* environments; it would be generally awkward to call Servo a host tool, and as such require rustb take --host instead of --target for crosses, despite the fact that it is a host 'tool' in most cases, except, that is, when performing cross builds.

For most projects though, only one subtarget will be used, which rustb will special case, putting the build outputs directly into target/_target name_/.

### C/CXX/Asm compilation:
In case of complex native deps, this will require rustb to serve as a C/CXX compiler middle man (much like ccache), loading the source dir -> final compiler flags map, applying the appropriate overrides to the provided compiler flags, and passing those on to the actual compiler detected by rustb before it began building. Additionally, when building C/CXX objects, rustb will block additional flags added to the compiler invocation through environmental variables unless it's through use as a middleman. Given one of rustb's many design goals is to save these build configuration details between builds and also promote easy overrides, this shouldn't be an issue.

### Dependencies:
All dependencies not found in the current project are recommended to have an URL address or a source dir relative path. While I have no intention of coding support for package management (if someone would like to take up that project, I would happily collaborate with him/her/them to design a convenient API), a Rust community plugin can use this address as the as the package key in a package repository. If expressed in address form, the address should be a valid URL pointing to a valid Git or Hg repository, with the prefix "git://" or "hg://" (I realize this makes "git://ssh://..." somewhat awkward; I'm open to suggestions), respectively (other prefixes will be possible with plugins) (FIXME: tags/branches/refs/etc).

As an aside, it would be desirable to have the option of expression of more complete extern crate expectations, like exact_ver, min_ver.

Without an address, rustb will require one to find the dependency via google or what-have-you and then build it oneself (rustb registers builds, so after that it should be a simple Q/A between the user && rustb). However, if the address is provided, rustb will add the crate and manage it as if it was part of the project, cloning the repository into <build dir>/extern/<repo name>, and adding the requisite overrides to use that repo as the source of the extern crate. Note that this is not the same as a gitmodule. As with all in-tree deps, any possible combination of configs for the dep are permissible. Such deps will be addressed as if they were part of the project.

### Misc Thoughts:
Rustc emulation provided with -c flag; ie: 
```bash
rustb -c $YOUR_RUSTC_FLAGS
```

I might not need to say this, but Makefile usage will be impossible to fully eliminate in Rust proper: at the very least, a script will still be needed to retrieve the snapshot to get the build started.

Native dependencies:

Simple .S/.c/.cc/.cpp linked with 
```rust
#[link(name = "<filename>", kind = "<ext>"]
```
configure &| make deps will stipulate use of a build crate.


# Rationales:
#### SIMD:

As part of a library for general purpose SIMD code I'm writing for Rust proper, I added expressions to the AST but, as a result of conflicting opinions, left the syntax of those expressions to syntax extensions. This caused problems for libsimd because in our current makefile system, target crates can't depend on host crates. Libsimd therefore can't depend on libsimd_syntax as it needs to; thumbing one's nose at the system results in undefined references in libsyntax when rustc tries to load the extension crate. Any syntax extension suffers from this; in the case of fourcc it works because no crate in the std crates depend on it, and therefore the resultant linker errors are hidden.

#### PNaCl:

Building for PNaCl requires some unique changes:
* Libuv doesn't build for it yet (usage of exotic C functions not supported by Newlib);
* Morestack can't be used (asm isn't allowed);
* Libgreen is out of the question (also due to asm usage);
* pnacl-clang in Pepper 33 doesn't support -shared as Pepper 31 does;
* pnacl-clang needs to be fed LLVM IR in order to get past the LLVM version differences;
* As per https://code.google.com/p/chromium/issues/detail?id=343594 straight pexe usage doesn't fly because of a linker error stemming from 'frem' usage (one has to run pnacl-translate with -save-temps and then manually run pnacl-nativeld with -lm and -lg to get a non-portable nexe (but at least that works, given the correct chrome opts)); and
* Being able to completely disable inlining would be nice.

As a result of these particularities, I need a way to affect the rust crate build pipeline and a way to easily override Things.

Given the scope and the general applicability of this crate to my fellow Oxides, I would greatly appreciate suggestions/guidance/coding help. Please, constructively tear this apart. 


*God, that was a novel.* Apologies.
