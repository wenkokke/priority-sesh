%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{Related work}\label{sec:related}
\paragraph{Session types in Haskell}

\citet{orchardyoshida17} discuss various approaches to implementing session types in Haskell. Their overview is reproduced below:
\begin{itemize}
\item
  \citet{neubauerthiemann04} give an encoding of first-order single-channel session-types with recursion;
\item
  Using \emph{parameterised monads}, \citet{pucellatov08} provide multiple channels, recursion, and some building blocks for delegation, but require manual manipulation of a session type context;
\item
  \citet{sackmaneisenbach08} provide an alternate approach where session types are constructed via a value-level witnesses;
\item
  \citet{imaiyuen10} extend \citet{pucellatov08} with delegation and a more user-friendly approach to handling multiple channels;
\item
  \citet{orchardyoshida16} use an embedding of effect systems into Haskell via graded monads based on a formal encoding of session-typed \textpi-calculus into PCF with an effect system;
\item
  \citet{lindleymorris16} provide a \emph{finally tagless} embedding of the GV session-typed functional calculus into Haskell, building on a linear \textlambda-calculus embedding due to \citet{polakow15}.
\end{itemize}
With respect to linearity, all works above---except \citet{neubauerthiemann04}---guarantee linearity by encoding a linear type context in the Haskell type system, which leads to a trade-off between having easy-to-write session types and having idiomatic programs.
We side-step this trade-off by relying on Linear Haskell to check linearity. Furthermore, our implementation supports all relevant features, including multiple channels, delegation, recursion, and highly idiomatic code.

With respect to deadlock freedom, none of the works above---except \citet{lindleymorris16}, guarantee deadlock freedom. However, \citet{lindleymorris16} guarantee deadlock freedom \emph{structurally}, by implementing GV. As discussed in~\cref{sec:introduction}, structure-based deadlock freedom is more restrictive than priority-based deadlock freedom, as it restricts communication graphs to \emph{trees}, whereas the priority-based approach allows cyclic structures of communication and more expressive programs.

\citet{orchardyoshida17} summarise the capabilities of the various implementations of session types in Haskell in a table, which we adapted in \cref{fig:table}, adding columns for the various versions of \texttt{priority-sesh}. In general, you may read \kinda as ``Kinda?'' and \deffo as a resounding ``Yes!'' For instance, \citet{pucellatov08} only provide \emph{partial} delegation, \citet{neubauerthiemann04}, \citet{pucellatov08}, and \citet{lindleymorris16} still need to use combinators instead of standard Haskell application, abstraction, or variables in \emph{some} places, and \citet{neubauerthiemann04} is only deadlock free on the technicality that they don't support multiple channels.

\begin{figure*}
  \centering
  \begin{tabular}{l || l l l l l l || l l l}
      \hline
    &
    &
    &
    &
    &
    &
    & \multicolumn{3}{c}{\texttt{priority-sesh}}
    \\
    & NT04
    & PT08
    & SE08
    & IYA10
    & OY16
    & LM16
    & \cref{sec:sesh}
    & \cref{sec:tree-sesh}
    & \cref{sec:priority-sesh}
    \\ \hline
    Recursion
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    &
    & \deffo
    & \deffo
    &
    \\
    Delegation
    &
    & \kinda
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    \\
    Multiple channels
    &
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    \\
    Idiomatic code
    & \kinda
    & \kinda
    &
    & \deffo
    & \deffo
    & \kinda
    & \deffo
    & \deffo
    & \deffo
    \\
    Easy-to-write session types
    & \deffo
    & \deffo
    & \deffo
    &
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    & \deffo
    \\
    Deadlock freedom
    & \kinda
    &
    &
    &
    &
    & \kinda
    &
    & \kinda
    & \deffo
    \\
    \emph{via process structure}
    & \kinda
    &
    &
    &
    &
    & \deffo
    &
    & \deffo
    &
    \\
    \emph{via priorities}
    &
    &
    &
    &
    &
    &
    &
    &
    & \deffo
  \end{tabular}
  \caption{Capabilities of various implementations of session types in Haskell~\cite[adapted from][]{orchardyoshida17}.}
  \label{fig:table}
\end{figure*}

\paragraph{Session types in other programming languages}
Session types have been integrated in other programming language paradigms.
\citet{JML15,ScalasY16,PadFuse} integrate \emph{binary} session types in the \emph{native} host language, without language extensions; this to avoid hindering session types use in practice.
To obtain this integration of session types without extensions \citet{ScalasY16,PadFuse}) combine \emph{static} typing of input and output actions with \emph{run-time} checking of linearity of channle usage.

Implementations of multiparty session types (MPST) are less common than binary implementations.
\citet{Scalas2017} integrate MPST in Scala building upon \citet{ScalasY16} and a continuation-passing style encoding of session types into linear types \citet{dardhagiachino12}.
There are several works on MPST in Java, detailed below.
\citet{SivaramakrishnanNZE10} implement MPST leveraging an extension of Java with session primitives;
\citet{HY16} develops a MPST-based API generation for Java leveraging CFSMs by \citet{Brand1983CFM}; and \citet{KDPG16} implement session types in the form of \emph{typestates} in Java.
\citet{DHHNY2015} implement MPST in Python and \citet{Fowler16,NY2017} in Erlang, focusing on {purely dynamic} MPST verification via run-time monitoring.
\citet{NY2017A,NBY2017} extend the work by
\citet{DHHNY2015} with actors and timed specifications.
\citet{LMMNSVY2015} adopt a dependently-typed MPST theory
to verify MPI programs.


\paragraph{Session types, linear logic and deadlock freedom}
The main line of work regarding deadlock freedom in session-typed systems is that of Curry-Howard correspondences with linear logic by \citet{girard87}.
\citet{CP10} defined a correspondence between session types and dual intuitionistic linear logic and \citet{wadler14} between session types and classical linear logic.
These works guarantee deadlock freedom \emph{by design} as the communication structures are restricted to trees.
\citet{dardhagay18} extend \citet{wadler14} with \emph{priorities} following \citet{kobayashi06,padovani14} thus allowing communication structures to include ``good'' cycles.
\citet{balzertoninho19} introduce \emph{sharing} and guarantee deadlock freedom via priorities.
All the above works deal with deadlock freedom in a session-typed $\pi$-calculus.
With regards to function languages, the original works on GV \cite{gayvasconcelos10,gayvasconcelos12} did not guarantee deadlock freedom. This was later addressed by \citet{lindleymorris15,wadler15} via syntactic restrictions where communication once again follows a tree structure. \citet{kokkedardha21} introduce PGV--Priority GV, by following \citet{dardhagay18} and allowing for more flexible programming in GV.

Other works of guaranteeing deadlock freedom in session-typed systems include the works by \citet{dezani-ciancaglinimostrous06}, where deadlock freedom is guaranteed by allowing only one active session at a time and by \citet{dezani-ciancagliniliguoro09progress}, where priorities \cite{kobayashi06} are used for correct interleaving of channels.
\citet{hondayoshida08} guarantee deadlock freedom \emph{within a single} session of MPST, but not for session interleaving.
\citet{kokke19} guarantees deadlock freedom of session types in Rust by enforcing a tree structure of communication actions.


\section{Discussion and future work}
We presented \texttt{priority-sesh}, an implementation of deadlock-free session types in Linear Haskell. Using Linear Haskell allows us to ensure linearity---or more accurate, have linearity guaranteed for us---without relying on complex type-level machinery. Consequently, we hit the sweet spot of easy-to-write session types and idiomatic code---in fact, probably \emph{the most} idiomatic code when compared with previous work, though the previous work predates Linear Haskell. Unfortunately, there are some drawbacks to using Linear Haskell. Most importantly, Linear Haskell is not very mature at this stage. For instance:
\begin{itemize}
\item
  Anonymous functions are assumed to be unrestricted rather than linear, meaning anonymous functions must be factored out into a let-binding or where-clause with \emph{at least} a minimal type signature such as |_ %1 -> _|.
\item
  There is no integration with \texttt{base} or popular Haskell packages, and given that \texttt{LinearTypes} is an extension, there likely won't be for quite a while. There's \texttt{linear-base}, which provides linear variants of many of the constructs in \texttt{base}. However, \texttt{linear-base} relies heavily on @unsafeCoerce@, which, \emph{ironically}, kills GHC's performance.
\item
  Generally, there is little integration with the Haskell ecosystem, \eg, one other contribution we made are the formatting directives for Linear Haskell in lhs2\TeX\footnote{\url{https://hackage.haskell.org/package/lhs2tex}}.
\end{itemize}
However, we believe that many of these drawbacks will disappear as the Linear Haskell ecosystem matures.

Our work also provides a library which guarantees deadlock freedom via \emph{priorities}, which allows for more flexible typing that previous work on deadlock freedom via \emph{process structure}.

In the future, we plan to address the issue of priority-polymorphic code and recursion session types in our implementation. (While the versions of our library in~\cref{sec:sesh,sec:tree-sesh} support recursion, that is not yet the case for the priority-based version in~\cref{sec:priority-sesh}.) This is a challenging task, as it requires complex reasoning about type-level naturals. We outlined various approaches in \cref{sec:priority-sesh}. However, an alternative would be to implement \texttt{priority-sesh} in Idris2~\cite{brady13,brady17}, which supports \emph{both} linear types \emph{and} complex type-level reasoning.
