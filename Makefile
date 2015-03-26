# LaTeX Makefile using latexmk

TEXC		= latexmk
TEXFLAGS	= -lualatex --shell-escape -synctex=1
SOURCE		= $(shell grep -l '\\documentclass' *.tex)
OBJECTS		= $(SOURCE:.tex=.bbl) $(SOURCE:.tex=.nav) $(SOURCE:.tex=.snm) $(SOURCE:.tex=.synctex.gz) $(SOURCE:.tex=.ptc)
REL_NAME	= mdtc_thesis


default: build

build: $(SOURCE)
	$(TEXC) $(TEXFLAGS) $(SOURCE)

clean:
	$(TEXC) -c

reset: clean
	rm -f $(OBJECTS)

continuous:
	$(TEXC) $(TEXFLAGS) $(SOURCE) -pdf -pvc -silent

release: build reset
	mv ${SOURCE:.tex=.pdf} $(REL_NAME:.pdf=).pdf
