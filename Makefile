# Path to Common Lisp compiler
LISP ?= sbcl

PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
DATADIR ?= $(PREFIX)/share
MANDIR ?= $(DATADIR)/man

all: tridashc man/tridashc.1.gz

sources = tridash.asd \
        src/package.lisp \
        src/interface.lisp \
        src/lexer.lisp \
        src/parser.lisp \
        src/operators.lisp \
        src/node.lisp \
        src/meta-node.lisp \
        src/node-table.lisp \
        src/modules.lisp \
        src/conditions.lisp \
        src/outer-nodes.lisp \
        src/builder.lisp \
        src/coalescer.lisp \
        src/prog-builder.lisp \
        src/main.lisp \
        src/util/package.lisp \
        src/util/cut.lisp \
        src/util/macros.lisp \
        src/builders/html/package.lisp \
        src/builders/html/builder.lisp \
        src/backends/javascript/package.lisp \
        src/backends/javascript/ast.lisp \
        src/backends/javascript/print.lisp \
        src/backends/javascript/analyze.lisp \
        src/backends/javascript/backend.lisp \
        src/backends/javascript/functions.lisp \
        src/backends/javascript/html.lisp

tridashc: $(sources)
	$(LISP) --load tridash.asd \
		--eval '(ql:quickload :tridash)' \
		--eval '(tridash::add-data-dir "${DATADIR}/")' \
		--eval '(tridash.backend.js::set-data-dir "${DATADIR}/")' \
		--eval '(asdf:make :tridash)' \
		--eval '(quit)'

man/tridashc.1.gz: man/tridashc.1
	gzip -fk man/tridashc.1

clean:
	rm -f tridashc
	rm -f man/tridashc.1.gz
	rm -f tridash.tar.gz

install: all
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 tridashc $(DESTDIR)$(BINDIR)

	install -d $(DESTDIR)$(DATADIR)/tridash/modules
	install -m 644 modules/* $(DESTDIR)$(DATADIR)/tridash/modules

	install -d $(DESTDIR)$(DATADIR)/tridash/backends/javascript
	install -m 644 src/backends/javascript/runtime/tridash.js $(DESTDIR)$(DATADIR)/tridash/backends/javascript

	install -d $(DESTDIR)$(MANDIR)/man1
	install -m 644 man/tridashc.1.gz $(DESTDIR)$(MANDIR)/man1


uninstall:
	rm -f $(DESTDIR)$(BINDIR)/tridashc
	rm -rf $(DESTDIR)$(DATADIR)/tridash
	rm -f $(DESTDIR)$(MANDIR)/man1/tridashc.1


distfiles = $(sources) \
	src/backends/javascript/runtime/tridash.js \
	modules/core.trd \
	modules/core.yml \
	man/tridashc.1 \
	tridashc \
	Makefile \
	README.md \
	LICENSE.md \
	install.sh \
	uninstall.sh

tridash.tar.gz: $(distfiles)
	tar -czf tridash.tar.gz $(distfiles)

dist: tridash.tar.gz

.PHONY: all clean install uninstall dist
