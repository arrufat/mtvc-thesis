#!/usr/bin/env bash
TEXFLAGS='-lualatex --shell-escape'
PGFFILE=$1
PDFFILE=`echo $PGFFILE | sed -e 's/\.tex/\.pdf/g'`
TMPFILE=tmp.tex

# wrap the PGF file inside a LaTeX document
echo "\\documentclass[11pt]{standalone}"                                    > ${TMPFILE}
echo "\\usepackage{amsmath, amssymb}"                                              >> ${TMPFILE}
echo "\\usepackage[T1]{fontenc}"                                                   >> ${TMPFILE}
echo "\\usepackage{mathptmx}"                                                      >> ${TMPFILE}
echo "\\usepackage[scaled]{helvet}"                                                >> ${TMPFILE}
echo "\\usepackage[usenames,dvipsnames]{xcolor}"                                   >> ${TMPFILE}
echo "\\usepackage{ifthen}"                                                        >> ${TMPFILE}
echo "\\usepackage{pgfplots,tikz}"                                                 >> ${TMPFILE}
echo "\\usetikzlibrary{decorations.markings,intersections}"                        >> ${TMPFILE}
echo "\\usepgfplotslibrary{fillbetween}"                                           >> ${TMPFILE}
echo "\\pgfplotsset{compat=1.12}"                                                  >> ${TMPFILE}
echo "\\begin{document}"                                                           >> ${TMPFILE}
echo "\\input{${PGFFILE}}"                                                         >> ${TMPFILE}
echo "\\end{document}"                                                             >> ${TMPFILE}

latexmk ${TEXFLAGS} ${TMPFILE}
mv tmp.pdf ${PDFFILE}
latexmk -c
rm ${TMPFILE}
