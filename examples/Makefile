all: sum.html limit.html

clean:
	rm -f sum.html
	rm -f limit.html

sum.html: sum.ui.html
	tridashc $< : node-name=ui -o $@ -p type=html -p main-ui=ui

limit.html: limit.ui.html
	tridashc $< : node-name=ui -o $@ -p type=html -p main-ui=ui

.PHONY: all clean
