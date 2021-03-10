%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{Introduction}\label{sec:introduction}

\todo{%
  Introduce \texttt{priority-sesh} using an example.}
\todo{%
  Discuss session types and deadlock freedom.}
\todo{%
  Briefly discuss Linear Haskell.}
\todo{%
  Briefly discuss session types in Haskell~\cite{pucellatov08,lindleymorris16,orchardyoshida17}}
\todo{%
  Briefly discuss Priority GV and Exceptional GV~\cite{padovaninovara15,fowlerlindley19,kokkedardha21}}

\paragraph{Contributions}
\begin{itemize}
\item
	First implementation of session types to take advantage of Linear Haskell for
	linearity checking (at least \citet{lindleymorris16} guarantee linearity via
	\citet{polakow15}).
\item
	First implementation of session types to guarantee deadlock freedom via
	priorities in Haskell, and perhaps the first embedding of priorities into an
	existing programming language ever (other implementations use deadlock freedom
	via tree structure~\cite{lindleymorris16,kokke19}).
\end{itemize}


(1) using linear haskell to make session types in haskell manageable, and (2) checking priorities