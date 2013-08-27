DESTDIR = /usr/local
LISP := sbcl

ifeq ($(LISP),sbcl)
FLAGS=--noinform --no-userinit --no-sysinit --disable-debugger
else
FLAGS=--quiet --no-init
endif

buildapp: command-line.lisp utils.lisp buildapp.lisp dumper.lisp package.lisp
	$(LISP) $(FLAGS) \
	  --eval "(require 'asdf)" \
	  --eval "(push \"$$(pwd)/\" asdf:*central-registry*)" \
	  --eval "(require 'buildapp)" \
          --eval "(buildapp::build-buildapp)" \
          --eval "#+sbcl (exit) #+ccl (quit)"

clean:
	rm -f buildapp *~ *.fasl *.lx32fsl

install: buildapp
	install -c -m 555 buildapp ${DESTDIR}/bin/buildapp
