%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{Introduction}\label{sec:introduction}

\todo{%
  Briefly discuss Linear Haskell.}
\todo{%
  Briefly discuss Exceptional GV~\cite{fowlerlindley19}}

Components in distributed systems must follow a predefined protocol in order to guarantee safety and lack of communication errors. In this context, session types were came to be. They are a type formalism used to specify and verify communication protocols between two or more communicating agents~\cite{honda93,takeuchihonda94,hondavasconcelos98,hondayoshida08}.
They have been defined for concurrent and functional models, among other paradigms. Most notably, they have been defined for the $\pi$-calculus~\cite{sangiorgiwalker01}---a process calculus for communication and concurrency, and for Good Variation (GV)~\cite{wadler14,lindleymorris15}---a linear concurrent $\lambda$-calculus.
The $\pi$-calculus has been a natural choice for session types as it is equipped with \emph{channels}, where communication takes place. Later on, channels were borrowed by GV, where the functional paradigm meets communication and concurrency.

Moving up from computational models, to practical implementations, session types have been also integrated in mainstream functional programming languages, most notably in Haskell.
\citet{pucellatov08} integrate session types in Haskell and provide multiple session channels, recursion and basis for channel mobility. \citet{lindleymorris16} provide an embedding of GV into Haskell following a linear $\lambda$-calculus embedding by \citet{polakow15}. A complete survey of session types in Haskell is produced by \citet{orchardyoshida17}.

When implementing session types in Haskell, or any programming language for that matters, the main and most challenging aspect is \emph{linearity}, which guarantees that a session channel will be owned by \emph{exactly one} communicating participant in parallel, while the channel itself will be used according to the sequence specified by the session type. All the works on session types in Haskell check linearity by an \emph{encoding} of linear typing contexts into the type system of the host language, which often makes for not easy to write session types and not idiomatic code.

The main reason for implementing session types in mainstream languages, is that they provide strong safety properties including communication safety---there will be no send and receive mismatch; session fidelity---communication follows the declared session type; and privacy of communicating channel, which is due to linearity.
Another strong and desirable property in distributed systems is \emph{deadlock freedom}, which states that communicating agents will eventually perform all their actions.
However deadlock freedom is a much stronger property and session types alone are not enough to guarantee it. This holds for both formal models and implementations of session types. In fact, in all implementations of session types in Haskell, none of them, except \citet{lindleymorris15}, studies deadlock freedom. Moreover, \citet{lindleymorris15} guarantee a very simple form of deadlock freedom, namely all programs that one could write have a tree structure of communication, hence deadlock freedom is guaranteed because it is impossible to write a program that deadlocks. While this is a step forward in this research direction, on the other hand it also restricts the kind of programs that one could write and removes many interesting and deadlock-free programs, just because they have cycles of communication.
To overcome these restrictions, recent work by \citet{padovaninovara15} and \citet{kokkedardha21} integrate the notion of \emph{priorities} \cite{kobayashi06}--- denoting the time an action becomes ready to fire---in a functional setting. Then, a priority-based type system checks that the order of priorities and their interleaving does not introduce any stuck cycles, leading to deadlocks.

In this work, we focus on linearity and deadlock freedom of session types in Haskell with the aim of
(i) \emph{checking linearity in the most elegant and simple way thus providing easy to write session types and highly idiomatic code};
and
(ii) \emph{guaranteeing deadlock freedom not only for tree-structured communication but also ``good'' cyclic structures thus providing a more flexible and expressive programming experience}.

In order to make session types manageable in Haskell and check priorities, our host language is Linear Haskell \cite{bernardyboespflug18}.


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