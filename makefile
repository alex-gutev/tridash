# Path to Common Lisp compiler
LISP ?= sbcl

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

build/tridashc: $(sources)
	$(LISP) --load tridash.asd \
		--eval '(ql:quickload :tridash)' \
		--eval '(asdf:make :tridash)' \
		--eval '(quit)'


all: build/tridashc

clean:
	rm build/tridashc

install: all
	cp build/tridashc /usr/local/bin
	mkdir -p /usr/local/lib/tridash/backends/javascript
	cp -R modules /usr/local/lib/tridash/modules
	cp src/backends/javascript/runtime/tridash.js /usr/local/lib/tridash/backends/javascript

uninstall:
	rm /usr/local/bin/tridashc
	rm -rf /usr/local/lib/tridash

.PHONY: all clean install uninstall
