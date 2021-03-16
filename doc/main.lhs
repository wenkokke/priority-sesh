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
	Session types are a type formalism used to specify and verify communication protocols between two or more communicating agents. They have been integrated in several paradigms including the functional one. In particular, session types have been defined for Good Variation (GV)---linear concurrent $\lambda$-calculus, on the formal side, and there are to date many implementations in Haskell, on the practical side.
  A challenging aspect of session types is when implementing them in mainstream languages, is \emph{linearity}. Works on session types in Haskell enforce linearity by mostly relying on Haskell's type system which then makes session types not easy to write and coding not idiomatic.

  In this work we implement session types in Linear Haskell, where linearity is enforced without complex type-level treatment and session types are easy to write. Consequently, we provide very idiomatic code. Moreover, we enforce deadlock freedom for the first time for session types in Haskell and do so via \emph{priorities}. These allow for more flexibility in programs as differently from previous work on deadlock freedom in GV, we allow cyclic structures of communications and not only trees.
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
