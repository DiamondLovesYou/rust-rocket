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

RUSTC ?= $(shell which rustc)
# if the user set RUSTC, make sure its absolute:
RUSTC := $(shell readlink -f $(RUSTC))

RUSTFLAGS += -g -L /usr/lib/llvm-3.4/lib

BINDGEN ?= $(shell readlink -f src/bindgen)
SQLITE  ?= $(shell readlink -f src/sqlite)

BUILD_OUT ?= $(shell readlink -f .)/build
RUSTFLAGS += --out-dir=$(BUILD_OUT)

LIBROCKET_OUTS := $(shell $(RUSTC) --crate-file-name src/lib/lib.rs)
ROCKET_OUT := $(shell $(RUSTC) --crate-file-name src/bin/main.rs)

.DEFAULT_GOAL := all
all: 	$(BUILD_OUT)/$(ROCKET_OUT) \
	$(BUILD_OUT)/$(word 1,$(LIBROCKET_OUTS))

$(BUILD_OUT):
	mkdir -p $(BUILD_OUT)

define DEP_RULES
# $(1) == the dep var prefix (ie BINDGEN)
# $(2) == the dep name
# $(3) == lib.rs, relative to the dep's root (ie $$(1)/)

-include $$(BUILD_OUT)/lib$(2).d

$(1)_OUT := $$(shell $$(RUSTC) --crate-type=dylib --crate-file-name $$($(1))/$(3))

$$(BUILD_OUT)/$$($(1)_OUT): $$(BUILD_OUT) Makefile $$(RUSTC)
	$$(RUSTC) $$(RUSTFLAGS) $$($(1))/$(3) --dep-info $$(BUILD_OUT)/lib$(2).d

DEPS += $$(BUILD_OUT)/$$($(1)_OUT)

endef

$(eval $(call DEP_RULES,BINDGEN,bindgen,lib.rs))
$(eval $(call DEP_RULES,SQLITE,sqlite,src/sqlite3/lib.rs))

-include $(BUILD_OUT)/librocket.d

$(BUILD_OUT)/$(word 1,$(LIBROCKET_OUTS)): src/lib/lib.rs Makefile $(BUILD_OUT) $(DEPS)
	$(RUSTC) $(RUSTFLAGS) --dep-info $(BUILD_OUT)/librocket.d $<

$(BUILD_OUT)/$(ROCKET_OUT): src/bin/main.rs Makefile $(BUILD_OUT) \
			    $(BUILD_OUT)/$(word 1,$(LIBROCKET_OUTS))
	$(RUSTC) $(RUSTFLAGS) --crate-type=bin -o $@ $<
