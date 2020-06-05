all: js wasm

# JavaScript Backend

js: app.html

app.html: ui.html
	tridashc ui.html : node-name=ui -o app.html -p type=html -p main-ui=ui


# WebAssembly Backend

wasm: wapp.html

wapp.html: ui.html
	tridashc $< : node-name=ui -o $@ -t wasm32 \
	-p type=html -p linkage=embed -p main-ui=ui


clean:
	rm -f app.html
	rm -f wapp.html

.PHONY: all js wasm clean
