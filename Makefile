all: index.html philosophy.html installation.html examples.html examples

index.html: home.adoc
	asciidoc -b html5 -a disable-javascript -a linkcss -a stylesdir=style -a icons $<
	mv home.html index.html

philosophy.html: philosophy.adoc
	asciidoc -b html5 -a disable-javascript -a linkcss -a stylesdir=style -a icons $<

installation.html: installation.adoc
	asciidoc -b html5 -a disable-javascript -a linkcss -a stylesdir=style -a icons $<

examples.html: examples.adoc
	asciidoc -b html5 -a disable-javascript -a linkcss -a stylesdir=style -a icons $<

examples:
	$(MAKE) -C examples

clean: clean-examples
	rm -f index.html
	rm -f philosophy.html
	rm -f installation.html
	rm -f examples.html

clean-examples:
	$(MAKE) -C examples clean

.PHONY: all clean examples clean-examples
