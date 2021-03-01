%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{Relation to Priority GV}

\todo{%
  The @priority-sesh@ library is based on a variant of Priority GV~\cite{kokkedardha21} with \emph{explicit} lower bounds on priorities, see~\cref{fig:pgv-typing}. The variant has the same strong guarantees as the original language, but has \emph{tighter} bounds on priorities at the cost of a more verbose type system. We've also added exceptions \`a la Exceptional GV~\cite{fowlerlindley19}. Who knows? It's probably fine.}

\input{fig-pgv-typing}

\todo{%
  The |Sesh p q| monad arises via a monadic reflection from Priority GV, see~\cref{fig:pgv-to-sesh-typing}.}

%include fig-pgv-to-sesh-typing.lhs
