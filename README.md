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
The advantage of `latexmk` is that it is in charge of rebuilding the document as many times as necessary, including the bibliography, the index and the cross-references.
The preferred `latexmk` back-end in this project is `lualatex`.

The `Makefile` options are described below:

### make (build)

It builds the document without (re)generating the plots.

### make figures

It generates PDFs for all figures and plots, so that they don't have to be created every time a modification is made to the document.

### make clean

It performs a simple cleaning of temporary files. It is equivalent to `latexmk -c`. The generated PDFs are kept.

### make reset

It cleans all the temporary files in the main directory. The generated PDFs are kept.

### make continuous

It watches the `.tex` files for modifications and rebuilds them automatically
whenever any of them is modified.

### make release

It performs:

- make figures (generates all figures and plots)
- make build (compiles the document)
- make reset (cleans up all temporary files)

The result should be a file named `mtvc-thesis.pdf`.



