all: index.html philosophy.html installation.html

index.html: home.adoc
	a2x --format xhtml --no-xmllint --stylesheet=home.css $<
	mv home.html index.html

philosophy.html: philosophy.adoc
	a2x --format xhtml --no-xmllint $<

installation.html: installation.adoc
	a2x --format xhtml --no-xmllint --icons $<


clean:
	rm -f index.html
	rm -f philosophy.html
	rm -f installation.html

.PHONY: all clean
