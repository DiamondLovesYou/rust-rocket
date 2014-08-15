# Copyright (c) 2014 Richard Diamond & contributors.
#
# This file is part of Rust Rocket.
#
# Rust Rocket is free software: you can redistribute it and/or modify
# it under the terms of the GNU Lesser General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# Rust Rocket is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU Lesser General Public License
# along with Rust Rocket. If not, see <http://www.gnu.org/licenses/>.

ifneq ($(SYSROOT),)
LD_LIBRARY_PATH:=$(shell readlink -f $(SYSROOT)/lib)$(LD_LIBRARY_PATH)
RUSTFLAGS += --sysroot=$(shell readlink -f $(SYSROOT))
RUSTC ?= $(shell readlink -f $(SYSROOT)/bin/rustc)
else
RUSTC ?= $(shell which rustc)
endif
# if the user set RUSTC, make sure its absolute:
RUSTC := $(shell readlink -f $(RUSTC))

RUSTFLAGS += -g -L /usr/lib/llvm-3.4/lib

SQLITE  ?= $(shell readlink -f src/sqlite)

BUILD_OUT ?= $(shell readlink -f .)/build
RUSTFLAGS += --out-dir=$(BUILD_OUT)

rwildcard = $(foreach d,$(wildcard $1*),$(call rwildcard,$d/,$2) $(filter $(subst *,%,$2),$d))

.DEFAULT_GOAL := all
all: $(BUILD_OUT) $(BUILD_OUT)/rocket.stamp
clean:
	touch Makefile

$(BUILD_OUT):
	mkdir -p $(BUILD_OUT)

define DEP_RULES
# $(1) == the dep var prefix (ie BINDGEN)
# $(2) == the dep name
# $(3) == source dir, relative to the dep's root (ie $$($(1))/)

$$(BUILD_OUT)/$(2).stamp: $$($(1))/$(3)                        \
			  $$(call rwildcard,$$($(1))$(3),*.rs) \
			  Makefile $$(RUSTC)
	export LD_LIBRARY_PATH; \
	$$(RUSTC) $$(RUSTFLAGS) $$< --crate-type=dylib
	touch $$@
DEPS += $$(BUILD_OUT)/$(2).stamp

endef

$(eval $(call DEP_RULES,SQLITE,sqlite,src/sqlite3.rs))

$(BUILD_OUT)/librocket.stamp: src/lib/lib.rs                 \
			      $(call rwildcard,src/lib,*.rs) \
			      Makefile $(BUILD_OUT) $(DEPS)  \
			      $(RUSTC)
	export LD_LIBRARY_PATH; \
	$(RUSTC) $(RUSTFLAGS) -L $(BUILD_OUT) $<
	touch $@

$(BUILD_OUT)/rocket.stamp: src/bin/main.rs                \
			   $(call rwildcard,src/bin,*.rs) \
			   Makefile $(BUILD_OUT)          \
			   $(BUILD_OUT)/librocket.stamp   \
			   $(RUSTC)
	export LD_LIBRARY_PATH; \
	$(RUSTC) $(RUSTFLAGS) -L $(BUILD_OUT) $<
	touch $@
