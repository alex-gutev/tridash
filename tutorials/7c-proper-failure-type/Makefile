all: js wasm

# JavaScript Backend

js: app.html

app.html: ui.html build.yml
	tridashc -b build.yml


# WebAssembly Backend

wasm: wapp.html

wapp.html: ui.html wbuild.yml
	tridashc -b wbuild.yml

clean:
	rm -f app.html
	rm -f wapp.html


.PHONY: all js wasm clean
