DESTDIR = /usr/local

buildapp: command-line.lisp utils.lisp buildapp.lisp dumper.lisp package.lisp
	sbcl --noinform --no-userinit --no-sysinit --disable-debugger \
	  --eval "(require 'asdf)" \
	  --eval "(require 'buildapp)" \
          --eval "(buildapp::build-buildapp)" \
          --eval "(exit)"

clean:
	rm -f buildapp *~ *.fasl

install: buildapp
	install -c -m 555 buildapp ${DESTDIR}/bin/buildapp
