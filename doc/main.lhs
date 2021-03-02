\documentclass[sigplan,screen]{acmart}
\bibliographystyle{ACM-Reference-Format}
\citestyle{acmauthoryear}

\let\Bbbk\undefined % acmart conflicts with lhs2TeX
%include polycode.fmt
%include linear.fmt
%include main.fmt

\input{preamble}
\input{preamble/pgv}
\input{preamble/sesh}

\begin{document}

\title{Session Types with Priority in Linear Haskell}

\author{Wen Kokke}
\affiliation{%
  \institution{University of Edinburgh}
  \city{Edinburgh}
  \country{Scotland}}
\email{wen.kokke@@ed.ac.uk}

\begin{abstract}
	To be written last\dots
\end{abstract}

\begin{CCSXML}
<ccs2012>
<concept>
<concept_id>10003752.10003790.10003801</concept_id>
<concept_desc>Theory of computation~Linear logic</concept_desc>
<concept_significance>300</concept_significance>
</concept>
<concept>
<concept_id>10003752.10003790.10011740</concept_id>
<concept_desc>Theory of computation~Type theory</concept_desc>
<concept_significance>300</concept_significance>
</concept>
</ccs2012>
\end{CCSXML}

\ccsdesc[300]{Theory of computation~Linear logic}
\ccsdesc[300]{Theory of computation~Type theory}

\keywords{session types, linear haskell, deadlock freedom}

\maketitle

%include introduction.lhs
%include sesh.lhs
%include pgv.lhs

\section{Related work}

\todo{%
  Comparison with Haskell implementations.}
\todo{%
  Comparison with other session types in PL:
  \emph{(i)} runtime checking of linearity;
  \emph{(ii)} static checking of linearity;
  \emph{(iii)} guarantee via an external tool, such as Scribble.}
\todo{%
  Checking deadlock freedom:
  - deadlock freedom via tree structure in~\cite{kokke19};
  - deadlock freedom via priorities in SILL [Balzer and Pfenning 2019].}

\clearpage
\bibliography{main}

\end{document}
