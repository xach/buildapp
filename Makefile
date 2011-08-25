DESTDIR = /usr/local

buildapp: command-line.lisp utils.lisp buildapp.lisp dumper.lisp package.lisp
	sbcl --disable-debugger --noinform --no-userinit --no-sysinit \
	  --eval "(require 'asdf)" \
	  --eval "(require 'buildapp)" \
          --eval "(buildapp::build-buildapp)" \
          --eval "(quit)"

clean:
	rm -f buildapp *~ *.fasl

install: buildapp
	install -c -m 555 buildapp ${DESTDIR}/bin/buildapp
