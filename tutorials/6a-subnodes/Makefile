all: app.html

app.html: ui.html build.yml
	tridashc -b build.yml

clean:
	rm -f app.html

.PHONY: all clean
