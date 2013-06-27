(define-skeleton latex-paper
  "Creates a standard SWA style presentation"
  ""
  "% Copyright (C) " (format-time-string "%Y") ", Tim Felgentreff
% All rights reserved.

\\documentclass[draft]{" (skeleton-read "Document class: ") "}
\\usepackage{makeidx}  % allows for indexgeneration
\\usepackage[pdftex]{graphicx} % PNGs
\\usepackage{amsmath, amssymb} % algebra
\\usepackage[english]{babel} % deutsch
% \\usepackage[ngerman]{babel} % deutsch
\\usepackage[utf8x]{inputenc}
\\usepackage[T1]{fontenc} 
\\usepackage{listings} % for sourcecode
\\usepackage{graphviz} % graphs
\\usepackage{array} % tables
\\usepackage{afterpage} % figures
\\usepackage{float} % figures
\\usepackage{paralist} % figures
\\usepackage[shortcuts]{extdash} % figures

\\lstset{%
  	language=Java,
	basicstyle=\\small,
	frame=single,
	emph={localId, Super, refines, layer},
	emphstyle={\\bfseries}
	}
\\restylefloat{figure}

\\usepackage{glossaries}
\\newcommand{\\ac}[1]{\\gls{#1}}
\\newcommand{\\acp}[1]{\\glspl{#1}}

% Acronyms
\\newacronym{api}{\\textsc{api}}{application programming interface}


\\newcommand\hairspace{\ifmmode\mskip1mu\else\kern0.08em\fi}
\\RequirePackage{xspace}
\\newcommand{\\eg}{e.\\hairspace{}g.,\\xspace}
\\newcommand{\\Eg}{E.\\hairspace{}g.,\\xspace}
\\newcommand{\\Ie}{I.\\hairspace{}e.,\\xspace}
\\newcommand{\\ie}{i.\\hairspace{}e.,\\xspace}
\\newcommand{\\cf}[1]{cf.\\,{#1}\\xspace}
\\newcommand{\\todo}[1]{\\framebox{\\textsc{todo} #1}}


\\begin{document}
\\frontmatter          % for the preliminaries
\\pagestyle{headings}  % switches on printing of running heads
\\mainmatter           % start of the contributions

\\title{TITLE}
\\subtitle{Subtitle}
\\titlerunning{Abbreviated Title}
\\author{" (skeleton-read "Author(s): ") "}
\\authorrunning{" (skeleton-read "Abbreviated Authors: ") "}   % abbreviated author list (for running head)
%
\\date{\\today}
%
%%%% modified list of authors for the TOC (add the affiliations)
% \\tocauthor{Tim Felgentreff (Hasso-Plattner-Institute)}
%
\\institute{Hasso-Plattner-Institut, Universität Potsdam, D-14482 Potsdam, Germany,\\\\
\\email{" (skeleton-read "Email(s) for HPI: ") "@hpi.uni-potsdam.de}}

\\maketitle              % typeset the title of the contribution

\\begin{abstract}
\\end{abstract}
%
\\section{Introduction}
\\subsection{Overview}

" _ "

% \\section*{Acknowledgments}
\\bibliographystyle{splncs.bst}
% Possibly change this
\\bibliography{" (expand-file-name (car reftex-default-bibliography)) "}
\\clearpage
\\end{document}

%%% Local Variables: 
%%% mode: latex
%%% TeX-master: t
%%% End: 
"
)

(define-skeleton latex-letter
  "Inserts a Latex letter skeleton into current buffer.
    This only makes sense for empty buffers."
  "Recipient: "
  "\\documentclass[a4paper]{letter}\n"
  "\\usepackage{german}\n"
  "\\usepackage[latin1]{inputenc}\n"
  "\\name{Tim Felgentreff}\n"
  "\\address{Tim Felgentreff \\\\ Seminarstr. 3 \\\\ 03044 Cottbus \\\\ Germany}\n"
  "\\begin{document}\n"
  "\\begin{letter}{" str | " *** Recipient *** " "}\n"
  "\\opening{" _ "}\n\n"
  "\\closing{Kind regards,}\n"
  "\\end{letter}\n"
  "\\end{document}\n")

(define-skeleton latex-brief
  "Inserts the g-brief skeleton"
  "\\documentclass[12pt,german]{g-brief}\n"
  "\\usepackage[latin1]{inputenc}\n"
  "\\usepackage{pdfpages}\n"
  "\\usepackage{times}\n"
  "\\fenstermarken\n"
  "\\trennlinien\n\n"
  "\\Name{Tim Felgentreff}\n"
  "\\Strasse{Patrizierweg 20}\n"
  "\\Ort{14480 Potsdam}\n"
  "\\Telefon{+49 (0)177 - 3381 411}\n"
  "\\EMail{timfelgentreff@gmail.com}\n"
  "\\Unterschrift{Tim Felgentreff}\n\n"
  "\\Adresse{Semmelhaack GmbH\\\\\n"
  "Am Tempelberg 7\\\\\n"
  "14469 Potsdam}\n\n"
  "\\Betreff{" _ "}\n"
  "\\Datum{\\today}\n"
  "\\Anrede{Sehr geehrte Damen und Herren,}\n"
  "\\Gruss{Mit freundlichen Gr\\\"u\ss{}en,}{1cm}\n"
  "% \\Anlagen{Anlage: ....}\n\n"
  "\\begin{document}\n"
  "\\begin{g-brief}\n"
  "\\end{g-brief}\n"
  "\\end{document}\n")

(define-skeleton beamer-presentation
  "Creates a standard SWA style presentation"
  ""
  "% Copyright (C) 2012, Tim Felgentreff

% Permission is hereby granted, free of charge, to any person obtaining a 
% copy of this software and associated documentation files (the \"Software\"), 
% to deal in the Software without restriction, including without limitation 
% the rights to use, copy, modify, merge, publish, distribute, sublicense, 
% and/or sell copies of the Software, and to permit persons to whom the 
% Software is furnished to do so, subject to the following conditions: 

% The above copyright notice and this permission notice shall be included in 
% all copies or substantial portions of the Software. 

% THE SOFTWARE IS PROVIDED \"AS IS\", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.	 IN NO EVENT SHALL 
% THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
% FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
% DEALINGS IN THE SOFTWARE. 

\\documentclass[14pt,utf8x]{beamer}

\\mode<presentation>
{
  \\usetheme{hpiswa}
}

\\usepackage[english]{babel}
\\usepackage{times}
\\usepackage[T1]{fontenc}

\\renewcommand{\\rmdefault}{phv} % Arial
\\renewcommand{\\sfdefault}{phv} % Arial

\\usepackage{listings}
\\alt<presentation>
{\\lstset{%
    basicstyle=\\ttfamily,
    commentstyle=\\slshape\\color{green!50!black},
    keywordstyle=\\bfseries\\color{orange},
    identifierstyle=\\color{blue},
    showstringspaces=false,
    moretexcs={draw,node,shade,shadedraw,filldraw,path},
    backgroundcolor=\\color{lightgray!40},
    stringstyle=\\color{orange}}
}

\\AtBeginSection[]
{
  \\begin{frame}
    \\frametitle{Outline}
    \\tableofcontents[currentsection]
  \\end{frame}
}

\\AtBeginSubsection[]
{
  \\begin{frame}
    \\frametitle{Outline}
    \\tableofcontents[currentsection,currentsubsection]
  \\end{frame}
}

\\title{" (skeleton-read "Title: ") "}
\\subtitle{" (skeleton-read "Subtitle: ") "}
\\author{Tim \\textsc{Felgentreff}}

\\institute
{
Hasso-Plattner-Institut Potsdam\\\\
Software Architecture Group\\\\
Prof. Dr. Robert Hirschfeld\\\\
\\url{http://www.hpi.uni-potsdam.de/swa/}
}

\\date{\\today{}}

\\begin{document}
\\frame[plain]{\\titlepage}

\\begin{frame}
  \\frametitle{Outline}
  \\tableofcontents
\\end{frame}

\\begin{frame}
  \\frametitle{" _ "}
\\end{frame}

\\begin{frame}[allowframebreaks]
  \\frametitle<presentation>{References}
  \\bibliographystyle{plain}
  \\bibliography{" (expand-file-name (car reftex-default-bibliography)) "}
\\end{frame}
\\end{document}

%%% Local Variables: 
%%% mode: latex
%%% TeX-master: t
%%% coding: utf-8
%%% TeX-PDF-mode: t
%%% ispell-local-dictionary: \"english\"
%%% End: \n")

(define-skeleton listings-lisp
  "\\lstdefinelanguage[CommonLisp]{Lisp}[]{Lisp}{morekeywords={cdr,car,setq, if, progn, lambda, ignore, declare, quote, make-instance}}\n"
  "\n"
  "\\lstnewenvironment{lisp}{%\n"
  "	\\lstset{\n"
  "          language=[CommonLisp]Lisp\n"
  "	}\n"
  "}{}\n"
  "\n"
  "\\lstnewenvironment{answer}{%\n"
  "	\\lstset{\n"
  "          language={},\n"
  "          basicstyle=\\ttfamily\\color{red},\n"
  "          keywordstyle=\\ttfamily\\color{red},\n"
  "          identifierstyle=\\ttfamily\\color{red},\n"
  "	}\n"
  "}{}\n"
  "\n")
