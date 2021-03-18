%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{Introduction}\label{sec:introduction}
Session types are a type formalism used to specify and verify communication protocols between two or more communicating agents~\cite{honda93,takeuchihonda94,hondavasconcelos98,hondayoshida08}.
They have been integrated in concurrent and functional paradigms, among others. Most notably, they have been defined for the $\pi$-calculus~\cite{sangiorgiwalker01}---a process calculus for communication and concurrency, and for Good Variation (GV)~\cite{wadler14,lindleymorris15}---a linear concurrent $\lambda$-calculus.
The $\pi$-calculus has been a natural choice for session types as it is equipped with \emph{channels}, where communication takes place. Later on, channels were borrowed by GV, where the functional paradigm meets communication and concurrency.

Moving from theoretical models to practical implementations, session types have also been integrated in functional programming languages, most notably in Haskell (a detailed presentation of session types in Haskell will be given in related works in \autoref{sec:related} and a complete survey is produced by \citet{orchardyoshida17}).

When implementing session types in Haskell---or any programming languages for that matters---the most challenging aspect is \emph{linearity}, which guarantees that a session channel will be \emph{owned by exactly one} communicating agent in parallel, while the channel itself might be used several times according to its session type. All works on session types in Haskell check linearity via an \emph{encoding} of linear typing contexts into Haskell's type system, leading to a trade-off between easy-to-write session types and idiomatic programs.

Moreover, these implementations only focus on the most basic features of session types and often ignore more advanced ones, such as channel delegation or deadlock freedom:
\citet{neubauerthiemann04} only provide single session channels; \citet{pucellatov08} provide multiple channels, but only the building blocks for channel delegation; \citet{imaiyuen10} extend \citet{pucellatov08} and provide full delegation. None of these works addresses deadlock freedom.
\citet{lindleymorris16} provide an embedding of GV into Haskell via a linear $\lambda$-calculus embedding by \citet{polakow15}. To the best of our knowledge, this is the only work that addresses deadlock freedom of session types in a functional setting, albeit in a simple form. Owing to GV, all programs must have \emph{tree-structured} communications, hence deadlock freedom is guaranteed by design as all \emph{cyclic-structured} programs are rejected. While this work makes a step forward to addressing more advanced properties, on the other hand it rejects many interesting programs which have {cyclic-structured} communications, but are deadlock free.


To overcome this restriction, recent works by \citet{padovaninovara15} and \citet[PGV]{kokkedardha21}, integrate the notion of \emph{priorities} \cite{kobayashi06}---denoting the \emph{time} in which an action is ready to fire---in a functional setting. A priority-based type system checks that the order of priorities does not introduce any cycles resulting in deadlocks. This increases expressivity, as now programs can contain cycles of communications, while the type system will reject the deadlocked ones.

Building upon the above observations, we thus pose our research questions below:

\textbf{Q1}:
\emph{Can we have easy-to-write session types, easy linearity checks and idiomatic code at the same time?}

\textbf{Q2}:
\emph{Can we address not only the main features of session types, but also advanced ones, such as full delegation or deadlock freedom of cyclic-structured programs?}


In this work, we address the above questions and provide an implementation of session types in Linear Haskell that answers positively both questions.
In order to address \textbf{Q1}, we chose Linear Haskell \cite{bernardyboespflug18} as our host language, which is the first implementation of linear types and type system in Haskell. This extension is backward compatible---existing programs in Haskell still typecheck in Linear Haskell; and linearity is \emph{non-invasive}---it allows for data types to store both linear and unrestricted values.
In order to address \textbf{Q2}, we have implemented full channel delegation and adopt priorities to guarantee deadlock freedom for cyclic-structured programs.


\textbf{Contributions} Our contributions are summarised below.

\begin{enumerate}
\item
We present Priority Sesh, an implementation of deadlock free session types in Linear Haskell which is:
\begin{itemize}
\item
	the \emph{first} implementation of session types to take advantage of Linear Haskell for linearity checking, and producing easy-to-write session types and highly idiomatic code; and
\item
	the \emph{first} implementation of session types in Haskell to guarantee deadlock freedom of cyclic-structured programs via \emph{priorities};
\item
	and the \emph{first} embedding of priorities into an existing mainstream programming language.
\end{itemize}

\item
We present an updated version of Priority GV \cite{kokkedardha21} typing system...\todo{Fill me in}

\item
We illustrate Priority Sesh via examples in \autoref{sec:main}.
\end{enumerate}
