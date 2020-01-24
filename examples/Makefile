examples = sum.html \
	limit.html \
	color.html

all: $(examples)

clean: $(examples:.html=.clean)


%.html: %.ui.html
	tridashc $< : node-name=ui -o $@ -p type=html -p main-ui=ui

%.clean:
	rm -f $(@:.clean=.html)


.PHONY: all clean
