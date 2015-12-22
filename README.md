# Multiple transforms for video coding

This is the `README` containing building instructions for the LaTeX document
from source.

## Dependencies

This document has been built on Arch Linux using the following packages:

- `bash` (for generating plots)
- `texlive-core`
- `texlive-latexextra`
- `texlive-pictures`
- `texlive-science`

## Building the document

The building process is done by a `Makefile` based on the `latexmk` command.
The forced `latexmk` back-end is `lualatex`.
The `Makefile` options are described below:

### make (build)

It builds the document without (re)generating the plots

### make figures

It generates PDFs for all figures and plots, so that they don't have to be created every time a modification is made to the document

### make clean

It performs a simple clean of temporary files. See `latexmk -c`

### make reset

It cleans all the temporary files in the main directory.

### make continuous

It watches the .tex files for modifications and rebuilds them automatically whenever one of the files is modified.

### make release

It performs:
- make figures
- make build
- make reset





