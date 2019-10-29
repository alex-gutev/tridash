all: app.html

app.html: build.yml util.trd app.trd ui.html
	tridashc -b build.yml

clean:
	rm -f app.html

.PHONY: all clean
