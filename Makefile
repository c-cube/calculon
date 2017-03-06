
all: index.html

%.html: %.adoc
	asciidoc $< > $@

