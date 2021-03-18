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
	Session types are used to specify and verify communication protocols between two or more communicating agents and have been integrated in many paradigms, including the functional one.
  On the theoretical side, session types have been defined most notably for Good Variation (GV)---linear concurrent $\lambda$-calculus. On the practical side, there are to date several implementations of session types in Haskell.
  A challenging task when implementing session types in mainstream programming languages, is checking \emph{linearity}. Implementations of session types in Haskell check linearity by encoding a linear typing context into Haskell's type system, which leads to a trade-off between having easy-to-write session types and idiomatic programs.

  In this work we bypass this trade-off and implement session types in Linear Haskell, where linearity is enforced without complex type-level operations, leading to easy-to-write session types \emph{and} highly idiomatic code. Moreover, we enforce deadlock freedom for session types in Haskell for the first time via \emph{priorities}. These allow for more flexibility in programs as differently from previous work on deadlock freedom in GV, we now allow cyclic structures of communications and not simply trees.
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
