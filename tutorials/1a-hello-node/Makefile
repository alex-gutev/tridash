all: js wasm

# JavaScript Backend

js: hello.html

hello.html: hello-ui.html
	tridashc hello-ui.html : node-name=ui -o hello.html -p type=html -p main-ui=ui


# WebAssembly Backend

wasm: whello.html

whello.html: hello-ui.html
	tridashc $< : node-name=ui -o $@ -t wasm32 \
	-p type=html -p linkage=embed -p main-ui=ui


clean:
	rm -f hello.html
	rm -f whello.html

.PHONY: all js wasm clean
