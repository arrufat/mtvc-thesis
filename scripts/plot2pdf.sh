#!/usr/bin/env bash
TEXFLAGS='-lualatex --shell-escape'
PGFFILE=$1
PDFFILE=`echo $PGFFILE | sed -e 's/\.tex/\.pdf/g'`
TMPFILE=tmp.tex

# wrap the PGF file inside a LaTeX document
echo "\\documentclass[11pt,a4paper]{standalone}"                                    > ${TMPFILE}
echo "\\usepackage{geometry}"                                                      >> ${TMPFILE}
echo "\\geometry{verbose,tmargin=2.5cm,bmargin=2.5cm,lmargin=2.5cm,rmargin=2.5cm}" >> ${TMPFILE}
echo "\\usepackage{amsmath, amssymb}"                                              >> ${TMPFILE}
echo "\\usepackage[T1]{fontenc}"                                                   >> ${TMPFILE}
echo "\\usepackage{mathptmx}"                                                      >> ${TMPFILE}
echo "\\usepackage[scaled]{helvet}"                                                >> ${TMPFILE}
echo "\\usepackage[usenames,dvipsnames]{xcolor}"                                   >> ${TMPFILE}
echo "\\usepackage{pgfplots,tikz}"                                                 >> ${TMPFILE}
echo "\\usetikzlibrary{decorations.markings,intersections}"                        >> ${TMPFILE}
echo "\\usepgfplotslibrary{fillbetween}"                                           >> ${TMPFILE}
echo "\\begin{document}"                                                           >> ${TMPFILE}
echo "\\input{${PGFFILE}}"                                                         >> ${TMPFILE}
echo "\\end{document}"                                                             >> ${TMPFILE}

latexmk ${TEXFLAGS} ${TMPFILE}
mv tmp.pdf ${PDFFILE}
latexmk -c
rm tmp.tex
