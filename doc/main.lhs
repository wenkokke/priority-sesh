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

\section{Conclusion, Related and Future Work}
\todo{%
  Comparison with other session types in PL:
  \emph{(i)} runtime checking of linearity;
  \emph{(ii)} static checking of linearity;
  \emph{(iii)} guarantee via an external tool, such as Scribble.}
\todo{%
  Checking deadlock freedom:
  - deadlock freedom via tree structure in~\cite{kokke19};
  - deadlock freedom via priorities in SILL [Balzer and Pfenning 2019].}

\paragraph{Session Types in Haskell}
\cite{orchardyoshida17} overview various approaches in the literature for session-typed communication in Haskell, with particular focus on linearity checks. Following this survey by \cite{orchardyoshida17} we discuss below the various approaches to implementing session types in Haskell with \emph{static} linearity checks.
\cite{neubauerthiemann04} introduce an encoding of session types with recursion for single channels only. Session types are first-order, meaning no channel delegation is possible.
\cite{pucellatov08} use \emph{parameterised monads} and instead adopt multiple
session channels, recursion, and a partial delegation, but context splits to deal with linearity are managed manually, which is cumbersome.
\cite{imai10} later on extend \cite{pucellatov08} and include full delegation and improve on the use of multiple channels. They provide a type inference for session types, but their types are hart to manipulate and understand.
\cite{SackmanEisenbach08} also use parametrised monads to implement session types, but differently from  \cite{pucellatov08}, they use parametrised monads to construct session type witnesses at the value level.
 \cite{orchardyoshida16} implement session types in Haskell via graded monads, guaranteeing their linearity, and leverage an encoding of session-typed $\pi$-calculus into PCF with an effect system. While types are easy to write, differently from \cite{imai10}, managing fresh channels is left to the user.
\cite{lindleymorris16} implement GV in Haskell, leveraging an embedding of a linear $\lambda$-calculus by \cite{polakow15}. They enforce linearity, they adopt the notion of context \emph{consumption} where contexts are divided into input and output contexts, the latter being the remained of what is consumed from the input context.
As discussed by \cite{orchardyoshida17} in the state of the art there is a trade-off between having manageable and easy to write session types and the amount of manual specification or manipulation of contexts, channel names etc.
Our work escapes this trade of and we have support for all relevant features including recursion, delegation, multiple channels, and highly idiomatic code, and since our implementation is in Linear Haskell, we do not need to deal with contexts.
Moreover, none of the works above, with exception of \cite{lindleymorris16}, guarantee deadlock freedom. The way \cite{lindleymorris16} deal with deadlock freedom is as a result of adopting GV and as discussed previously, it is more restrictive than our work in that it only allows tree structures of communication, whether we allow cyclic structures due to the use of priorities. 

\paragraph{Session Types in other Programming Paradigms}

\paragraph{Session Types and Deadlock Freedom}

\paragraph{Conclusion and Future Work}



\clearpage
\bibliography{main}

\end{document}
