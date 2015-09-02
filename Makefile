# LaTeX Makefile using latexmk

TEXC		= latexmk
TEXFLAGS	= -lualatex --shell-escape -synctex=1 -quiet
SOURCE		= $(shell grep -l '\\documentclass' *.tex)
OBJECTS		= $(SOURCE:.tex=.bbl) $(SOURCE:.tex=.nav) $(SOURCE:.tex=.snm) $(SOURCE:.tex=.synctex.gz) $(SOURCE:.tex=.ptc)
MKFIGURES	= ./scripts/fig2pdf.sh
FIGURES		= $(wildcard figures/*.tex)
REL_NAME	= mtvc_thesis

.PHONY: figures

default: build

build: $(SOURCE)
	$(TEXC) $(TEXFLAGS) $(SOURCE)
	@echo

figures: $(figures)
	$(foreach fig,$(FIGURES), $(MKFIGURES) $(fig);)

clean:
	$(TEXC) -c

reset: clean
	rm -f $(OBJECTS)

continuous:
	$(TEXC) $(TEXFLAGS) $(SOURCE) -pdf -pvc -silent

release: figures build reset
	mv ${SOURCE:.tex=.pdf} $(REL_NAME:.pdf=).pdf
