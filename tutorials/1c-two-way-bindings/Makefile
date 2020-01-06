all: app.html

app.html: ui.html
	tridashc ui.html : node-name=ui -o app.html -p type=html -p main-ui=ui

clean:
	rm -f app.html

.PHONY: all clean
