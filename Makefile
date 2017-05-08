.PHONY: all

BASEDIR ?= $(shell dirname $(realpath $(lastword $(MAKEFILE_LIST))))
DESTDIR ?= /usr/local

LISPS :=

SBCL ?= sbcl
SBCL_PATH := $(shell which $(SBCL) 2>/dev/null)
ifeq "$(.SHELLSTATUS)" "0"
LISPS += sbcl
endif
SBCL_FLAGS := --noinform --no-userinit --no-sysinit --disable-debugger

CCL ?= ccl
CCL_PATH := $(shell which $(CCL) 2>/dev/null)
ifeq "$(.SHELLSTATUS)" "0"
LISPS += ccl
endif
CCL_FLAGS := --quiet --no-init

FORMS :=--eval "(require 'asdf)"				\
	--eval "(push \"$(BASEDIR)/\" asdf:*central-registry*)"	\
	--eval "(require 'buildapp)"

all: $(addprefix buildapp-,$(LISPS))

buildapp-sbcl: command-line.lisp utils.lisp buildapp.lisp dumper.lisp package.lisp
	$(SBCL) $(SBCL_FLAGS) $(FORMS) --eval "(buildapp::build-buildapp \"$@\")" --eval "(exit)"

buildapp-ccl: command-line.lisp utils.lisp buildapp.lisp dumper.lisp package.lisp
	$(CCL) $(CCL_FLAGS) $(FORMS) --eval "(buildapp::build-buildapp \"$@\")" --eval "(quit)"

clean:
	rm -f $(addprefix buildapp-,$(LISPS)) *~ *.fasl *.lx32fsl

install: buildapp $(addprefix buildapp-,$(LISPS))
	install -d $(DESTDIR)/bin
	install -c -m 555 $^ $(DESTDIR)/bin/
