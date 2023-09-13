# Multiple transforms for video coding

This is the `README` containing building instructions for the LaTeX document
from source.

The official version has been to the archiving system of the University of Rennes 1 and can be downloaded here:
https://hal-univ-rennes1.archives-ouvertes.fr/UR1-THESES/tel-01303759

## Context

Nowadays, video services play a major role in information exchanges around the world. Despite the
progress achieved in the last years with video coding standards, improvements are still required as new
formats emerge: as High Frame Rate (HFR), High Dynamic Range (HDR) and High Definition (HD)
formats become more and more common, new needs for video compression appear that must exploit
properties in these domains to achieve higher compression rates.

All these formats are made realistic in terms of service deployment thanks to the fact that around
every 10 years, the coding efficiency doubles for equivalent quality. In 2003, the H.264/MPEG-4 AVC
standard was defined, providing a compression rate of around 50% with regards MPEG-2 video, defined
in 1993. In January 2013, the HEVC standard was released, which outperforms H.264/MPEG-4 AVC
by 50% in terms of bitrate savings for equivalent perceptual quality.
The work carried out in this thesis started in November 2012, with the HEVC standard almost completely
defined. Consequently, the focus has been put on improving HEVC with new techniques that
could be adopted in a future standard, tentatively for around 2020. Recently, ITU and ISO, through
their respective groups VCEG and MPEG, have started working towards a possible future video coding
standard for that time frame.

Being at the beginning of the post-HEVC era, the first steps in this thesis strive to achieve important
bitrate savings over HEVC by relaxing complexity constraints.
This thesis is strongly connected to the standardisation context. The first exploratory direction points
towards finding new techniques regarding the role of transforms in video coding, such as different
transform design methods and the usage of multiple transforms adapted to the nature of video coding signals.
Then, the studies move towards making these new techniques involving multiple transforms admissible
in a standardisation context, which implies having reasonable impact on standardisation aspects, such as
complexity, especially on the decoder side.

## Dependencies

This document has been built on Arch Linux using the following packages:

- `gnuplot` (for generating plots)
- `texlive-basic`
- `texlive-bin`
- `texlive-binextra`
- `texlive-fontsrecommended`
- `texlive-langfrench`
- `texlive-latex`
- `texlive-latexextra`
- `texlive-latexrecommended`
- `texlive-luatex`
- `texlive-mathscience`
- `texlive-pictures`

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
