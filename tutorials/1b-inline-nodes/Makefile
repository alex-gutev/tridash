all: hello.html

hello.html: hello-ui.html
	tridashc hello-ui.html : node-name=ui -o hello.html -p type=html -p main-ui=ui

clean:
	rm -f hello.html

.PHONY: all clean
