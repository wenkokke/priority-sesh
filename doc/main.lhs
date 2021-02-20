\documentclass[sigplan,screen]{acmart}

%%% lhs2TeX directives:
\let\Bbbk\undefined
%include polycode.fmt

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

\section{Overview}
\begin{itemize}
\item an example using \texttt{priority-sesh}
\item introduction to \texttt{priority-sesh}
\item correspondence to PGV (via a monadic reification)
\end{itemize}

\end{document}