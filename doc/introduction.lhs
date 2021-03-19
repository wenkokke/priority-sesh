%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{Introduction}\label{sec:introduction}
Session types are used to specify and verify communication protocols~\cite{honda93,takeuchihonda94,hondavasconcelos98,hondayoshida08}. They've been studied extensively in the context of the $\pi$-calculus~\cite{sangiorgiwalker01}, a process calculus for communication an concurrency, and in the context of concurrent $\lambda$-calculi such as the GV family of languages~\cite[``Good Variation'',][]{gayvasconcelos10,wadler14,lindleymorris15,fowlerlindley19}.

Session types have been implemented in various programming languages. We give a detailed overview in \cref{sec:related}, and \citet{orchardyoshida17} provide a complete survey of session type implementations \emph{in Haskell}.

The main difficulty when implementing session types in most programming languages is \emph{linearity}, \ie, the guarantee that each channel endpoint is used \emph{exactly once}. There's several different approaches to guaranteeing linearity, but the main distinction is between \emph{dynamic} and \emph{static} usage checks. In the former, using a channel endpoint a second time simply throws a runtime error. In the latter, usage is \emph{somehow} encoded into the type system of the host language, usually by encoding the entire linear typing environment into the type system using a parameterised or graded monad. Unfortunately, this often results in a trade-off between easy-to-write session types and idiomatic programs.

Moreover, these implementations only focus on the most basic features of session types and often ignore more advanced ones, such as channel delegation or deadlock freedom: \citet{neubauerthiemann04} only provide single session channels; \citet{pucellatov08} provide multiple channels, but only the building blocks for channel delegation; \citet{imaiyuen10} extend \citet{pucellatov08} and provide full delegation. None of these works addresses deadlock freedom.
\citet{lindleymorris16} provide an implementation of GV into Haskell via building on the work of \citet{polakow15}. To the best of our knowledge, this is the only work that addresses deadlock freedom of session types in Haskell, albeit in a simple form. In GV, all programs must have \emph{tree-shaped} process structures. (The process structure of a program is an undirected graph, where nodes represent processes, and edges represent the channels connecting them.) Therefore, deadlock freedom is guaranteed by design: session types rule out deadlocks over a single channel, and the tree-restriction rules out sharing multiple channels between two processes. While \citet{lindleymorris16} manage to implement more advanced properties, the tree-restriction rules out many interesting programs which have \emph{cyclic} process structure, but are deadlock free.

Recent works by \citet{padovaninovara15} and \citet[PGV,][]{kokkedardha21} integrate \emph{priorities}~\cite{kobayashi06,padovani14} into functional languages. Priorities abstractly represent the time at which a communication action happens. Priority-based type systems check that there are no cycles in the communication graph. (The communication graph is a directed graph where nodes represent dual communication actions, and directed edges represent one action must happen before another. We explore this in more detail in \cref{sec:priority-sesh}.) Such type systems are \emph{much} more expressive, as we can allow programs to have \emph{cyclic} process structure, as long as they have an \emph{acyclic} communication graph.

With the above in mind, our research goals are as follows:
\begin{description}
\item[Q1]
  Can we have easy-to-write session types, easy linearity checks and idiomatic code at the same time?
\item[Q2]
  Can we address not only the main features of session types, but also advanced ones, such as full delegation, recursion, and deadlock freedom of programs with cyclic process structure?
\end{description}
Our \texttt{priority-sesh} library answers both questions \emph{mostly} positively! We cheat a little on \textbf{Q1}, and implement our library using Linear Haskell~\cite{bernardyboespflug18}, which has native support for linear types, which saves us the overhead of encoding linearity. (This is mildly unfair, as the related work predates Linear Haskell.) However, the resulting session type library presented in \cref{sec:sesh,sec:tree-sesh} has both easy-to-write session types, easy linearity checks, and idiomatic code. Moving to \textbf{Q2}, the library has full delegation, recursion, and the variant in \cref{sec:tree-sesh} even guarantees of deadlock freedom, albeit by restricting the process structure to trees and forests.

This leaves deadlock freedom of programs with cyclic processes structure, and priorities. In \cref{sec:priority-sesh}, we implement a third variant of session types which uses priorities to ensure deadlock freedom. The easy-of-use suffers a little as a consequence, as the programmer has to manually write priorities, though this isn't a \emph{huge} inconvenience. Unfortunately, GHC's ability to reason about type-level naturals currently isn't powerful enough to allow the programmer to easily write priority-polymorphic code, which is required for \emph{recursion}.

\paragraph{Contributions}
In~\cref{sec:main}, we present Priority Sesh, an implementation of deadlock free session types in Linear Haskell which is:
\begin{itemize}
\item
  the \emph{first} implementation of session types to take advantage of Linear Haskell for linearity checking, and producing easy-to-write session types and highly idiomatic code;
\item
  the \emph{first} implementation of session types in Haskell to guarantee deadlock freedom of programs with cyclic process structure via \emph{priorities}; and
\item
  the \emph{first} embedding of priorities into an existing mainstream programming language.
\end{itemize}
In~\cref{sec:pgv}, we:
\begin{itemize}
\item
  present a variant of Priority GV~\cite{kokkedardha21}---the calculus upon which Priority Sesh is based---with asynchronous communication and session cancellation following~\citet{fowlerlindley19} and \emph{explicit} lower bounds on the sequent, rather than lower bounds inferred from the typing environment; and
\item
  show that Priority Sesh is related to Priority GV via monadic reflection.
\end{itemize}
