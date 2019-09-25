all: index.html philosophy.html installation.html

index.html: home.adoc
	asciidoc -b html5 -a disable-javascript -a linkcss -a stylesdir=style -a icons $<
	mv home.html index.html

philosophy.html: philosophy.adoc
	asciidoc -b html5 -a disable-javascript -a linkcss -a stylesdir=style -a icons $<

installation.html: installation.adoc
	asciidoc -b html5 -a disable-javascript -a linkcss -a stylesdir=style -a icons $<


clean:
	rm -f index.html
	rm -f philosophy.html
	rm -f installation.html

.PHONY: all clean
