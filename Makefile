.PHONY: all unit clean

all: 
	jbuilder build @install

test:
	jbuilder runtest

clean:
	jbuilder clean

README.html: README.md
	pandoc -c style.css -f markdown_github < README.md > README.html
