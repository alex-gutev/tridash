# Path to Common Lisp compiler
LISP ?= sbcl

build/tridashc:
	$(LISP) --load tridash.asd \
		--eval '(ql:quickload :tridash)' \
		--eval '(asdf:make :tridash)' \
		--eval '(quit)'


all: build/tridashc

clean:
	rm build/tridashc

install: all
	cp build/tridashc /usr/local/bin
	mkdir -p /usr/local/lib/tridash
	cp -R modules /usr/local/lib/tridash/modules

uninstall:
	rm /usr/local/bin/tridashc
	rm -rf /usr/local/lib/tridash

.PHONY: all clean install uninstall
