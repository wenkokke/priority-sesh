\documentclass[sigplan,screen]{acmart}
\citestyle{acmauthoryear}

\let\Bbbk\undefined % acmart conflicts with lhs2TeX
%include polycode.fmt
%include linear.fmt
%include main.fmt

\input{preamble}
\input{preamble/pgv}
\input{preamble/sesh}

\begin{document}
\sloppy

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


\copyrightyear{2021}
\acmYear{2021}
\setcopyright{acmlicensed}\acmConference[Haskell '21]{Proceedings of the 14th ACM SIGPLAN International Haskell Symposium}{August 26--27, 2021}{Virtual, Republic of Korea}
\acmBooktitle{Proceedings of the 14th ACM SIGPLAN International Haskell Symposium (Haskell '21), August 26--27, 2021, Virtual, Republic of Korea}
\acmPrice{15.00}
\acmDOI{10.1145/3471874.3472979}
\acmISBN{978-1-4503-8615-9/21/08}

\begin{abstract}
  \emph{Priority Sesh} is a library for session-typed communication in Linear Haskell which offers strong compile-time correctness guarantees. Priority Sesh offers two deadlock-free APIs for session-typed communication. The first guarantees deadlock freedom by restricting the process structure to trees and forests. It is simple and composeable, but rules out cyclic structures. The second guarantees deadlock freedom via priorities, which allows the programmer to safely use cyclic structures as well.

  Our library relies on Linear Haskell to guarantee linearity, which leads to easy-to-write session types and more idiomatic code, and lets us avoid the complex encodings of linearity in the Haskell type system that made previous libraries difficult to use.
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

\begin{acks}
  We thank Simon Fowler and April Gon\c{c}alves for comments on the manuscript.
\end{acks}

\bibliographystyle{ACM-Reference-Format}
\bibliography{main}

\end{document}
