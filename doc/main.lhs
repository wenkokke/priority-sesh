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

\title{Deadlock-Free Session Types in Linear Haskell}

\author{Wen Kokke}
\affiliation{%
  \institution{University of Edinburgh}
  \city{Edinburgh}
  \country{Scotland}}
\email{wen.kokke@@ed.ac.uk}

\author{Ornela Dardha}
\affiliation{%
  \institution{University of Glasgow}
  \city{Glasgow}
  \country{Scotland}}
\email{ornela.dardha@@glasgow.ac.uk}


\begin{abstract}
	Session types model communication protocols between two or more communicating agents. They have been integrated in many programming paradigms, including the functional one. To date, there are several implementations of session types in Haskell. 
  A challenging task when implementing session types is \emph{linearity}. Implementations of session types in Haskell check linearity either dynamically or statically by encoding a linear typing environment into Haskell's type system, which leads to a trade-off between having easy-to-write session types and idiomatic programs. Moreover, these implementations only focus on the most basic properties of session types and often ignore important ones, such as \emph{deadlock freedom}.

  In this work, we bypass the aforementioned trade-off and for the first time implement session types in Linear Haskell, where linearity is checked without introducing complex type-level operations. This leads to easy-to-write session types \emph{and} highly idiomatic code. In addition, we enforce deadlock freedom for the first time via \emph{priorities} for session types in Haskell. Priorities introduce more expressivity and flexibility in programs as differently from previous work, they allow cyclic process structures and not simply tree structures.
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
%include conclusion.lhs

\clearpage
\bibliography{main}

\end{document}
