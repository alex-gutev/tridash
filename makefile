# Path to Common Lisp compiler
LISP ?= sbcl

PREFIX ?= /usr/local
BINDIR ?= $(PREFIX)/bin
DATADIR ?= $(PREFIX)/share

all: tridashc

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


clean:
	rm -f tridashc

install: all
	install -d $(DESTDIR)$(BINDIR)
	install -m 755 tridashc $(DESTDIR)$(BINDIR)

	install -d $(DESTDIR)$(DATADIR)/tridash/modules
	install -m 644 modules/* $(DESTDIR)$(DATADIR)/tridash/modules

	install -d $(DESTDIR)$(DATADIR)/tridash/backends/javascript
	install -m 644 src/backends/javascript/runtime/tridash.js $(DESTDIR)$(DATADIR)/tridash/backends/javascript


uninstall:
	rm -f $(DESTDIR)$(BINDIR)/tridashc
	rm -rf $(DESTDIR)$(DATADIR)/tridash

.PHONY: all clean install uninstall
