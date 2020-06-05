all: js wasm

# JavaScript Backend

js: app.html

app.html: build.yml ui.html app.trd validation.trd
	tridashc -b $<


# WebAssembly Backend

wasm: wapp.html

wapp.html: wbuild.yml ui.html app.trd validation.trd
	tridashc -b $<

clean:
	rm -f app.html
	rm -f wapp.html


.PHONY: all js wasm clean
