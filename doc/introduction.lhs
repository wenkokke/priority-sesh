%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{Introduction}\label{sec:introduction}

\todo{%
  Introduce \texttt{priority-sesh} using an example.}
\todo{%
  Briefly discuss Linear Haskell.}
\todo{%
  Briefly discuss Exceptional GV~\cite{fowlerlindley19}}

Communication in distributed systems must follow a predefined protocol to guarantee safety and lack of communication errors. In this context, session types were introduced. They are a type formalism used to specify and verify communication protocols between two or more communicating agents~\cite{honda93,takeuchihonda94,hondavasconcelos98,hondayoshida08}.
They have been defined in both concurrent and functional computation models. Most notably, they have been defined for the $\pi$-calculus~\cite{sangiorgiwalker01}---a process calculus for communication and concurrency, and for Good Variation (GV)~\cite{wadler14,lindleymorris15}---a linear concurrent $\lambda$-calculus.
The $\pi$-calculus was a natural choice as it is equipped with \emph{channels}, which were then borrowed by GV, where the functional paradigm meets communication and concurrency.
Moving up from computational models, session types have been also integrated in mainstream functional programming languages, including Haskell.

\citet{pucellatov08} integrate session types in Haskell and provide multiple session channels, recursion and basis for channel mobility. \citet{lindleymorris16} provide an embedding of GV into Haskell following a linear $\lambda$-calculus embedding by \citet{polakow15}. A full survey of session types in Haskell is given by \citet{orchardyoshida17},

When it comes to implementing session types in Haskell, or any programming language for that matters, the main ingredient to pay attention to is \emph{linearity}, which guarantees that a session channel will be owned by \emph{exactly one} communicating participant, while the channel will be used according to the structure specified by the session type. All the above works--and the rest in the related work--on session types in Haskell, check linearity by an \emph{encoding} of linear typing contexts into the language's type system, which often makes for not easy to write session types.

When it comes to session types and guarantees they provide, one such strong property is that of deadlock freedom which states that communicating agents will eventually perform all their actions.
Session types elegantly guarantee communication safety, session fidelity and privacy of communicating channels, due to linearity, however deadlock freedom is a much stronger property and session types alone are not enough. This situation is also brought in the various implementation. In fact, in all implementations of session types in Haskell, none of them, except \citet{lindleymorris15}, studies deadlock freedom. Moreover \citet{lindleymorris15} guarantee a very simple form of deadlock freedom, namely all programs have a tree structure of communication, hence deadlock freedom is guaranteed because it is impossible to write a program that deadlocks. While this is a strong guarantee, it also restricts the sort of programs that one could write and removes many interesting and deadlock-free programs just because they have cycles of communication.
To overcome these restrictions, recent work by \citet{padovaninovara15} and later \citet[PGV]{kokkedardha21} introduce the notion of \emph{priorities} attached channel types and denoting the time an action becomes ready to fire. Then, a priority-based type system checks that the order of priorities and their interleaving does not introduce any stuck cycles, thus leading to deadlocks.

In this work, we focus on linearity and deadlock freedom of session types in Haskell with the aim of

(i) dealing with linearity in the most elegant and simple way and providing highly idiomatic code;and (ii) guaranteeing deadlock freedom not only for tree-structured communication but also ``good'' cyclic structures and providing more flexible and expressive programming experience.

In order to make session types manageable in Haskell and check priorities, we use Linear Haskell.


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