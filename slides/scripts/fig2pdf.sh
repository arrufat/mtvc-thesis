#!/usr/bin/env bash
TEXFLAGS='-lualatex --shell-escape'
PGFFILE=$1
PDFFILE=`echo $PGFFILE | sed -e 's/\.tex/\.pdf/g'`
TMPFILE=tmp.tex

# wrap the PGF file inside a LaTeX document
echo "\\documentclass[11pt,a4paper]{standalone}"                                    > ${TMPFILE}
if [[ ${PGFFILE} == *"_plot.tex" ]]; then
echo "\\usepackage{geometry}"                                                      >> ${TMPFILE}
echo "\\geometry{verbose,tmargin=2.5cm,bmargin=2.5cm,lmargin=2.5cm,rmargin=2.5cm}" >> ${TMPFILE}
fi
echo "\\usepackage[T1]{fontenc}"                                                   >> ${TMPFILE}
# echo "\\usepackage{mathptmx}"                                                      >> ${TMPFILE}
echo "\\usepackage[helvet]{sfmath}"                                                >> ${TMPFILE}
echo "\\usepackage[scaled]{helvet}"                                                >> ${TMPFILE}
echo "\\renewcommand*\\familydefault{\\sfdefault}"                                 >> ${TMPFILE}
echo "\\usepackage{amsmath, amssymb}"                                              >> ${TMPFILE}
echo "\\usepackage[usenames,dvipsnames]{xcolor}"                                   >> ${TMPFILE}
echo "\\usepackage{ifthen}"                                                        >> ${TMPFILE}
echo "\\usepackage{pgfplots,tikz}"                                                 >> ${TMPFILE}
echo "\\usetikzlibrary{shapes,arrows,fit,calc,decorations.markings,intersections}" >> ${TMPFILE}
echo "\\usepgfplotslibrary{fillbetween}"                                           >> ${TMPFILE}
echo "\\pgfplotsset{compat=1.12}"                                                  >> ${TMPFILE}
echo "\\begin{document}"                                                           >> ${TMPFILE}
echo "\\input{${PGFFILE}}"                                                         >> ${TMPFILE}
echo "\\end{document}"                                                             >> ${TMPFILE}

latexmk ${TEXFLAGS} ${TMPFILE}
mv tmp.pdf ${PDFFILE}
latexmk -c
rm ${TMPFILE}
