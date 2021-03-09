\section{Conclusion, Related and Future work}

\todo{%
  Comparison with other session types in PL:
  \emph{(i)} runtime checking of linearity;
  \emph{(ii)} static checking of linearity;
  \emph{(iii)} guarantee via an external tool, such as Scribble.}
\todo{%
  Checking deadlock freedom:
  - deadlock freedom via tree structure in~\citet{kokke19};
  - deadlock freedom via priorities in SILL [Balzer and Pfenning 2019].}

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
All works above---with the exception of \citet{neubauerthiemann04}---guarantee linearity by encoding a linear type context in the Haskell type system, which leads to a trade-off between having easy-to-write session types and having idiomatic programs.

We side-step this trade-off by relying on Linear Haskell to check linearity. Furthermore, our implementation supports all relevant features, including multiple channels, delegation, recursion, and highly idiomatic code.

Moreover, none of the works above guarantee deadlock freedom, with the exception of \citet{lindleymorris16}, who guarantee deadlock freedom \emph{structurally}, by implementing GV. As discussed in~\cref{sec:TODO}, structure-based deadlock freedom is more restrictive than priority-based deadlock freedom, as it restricts communication graphs to \emph{trees}, whereas the priority-based approach allows cyclic structures.


\paragraph{Session types in other programming languages}
Session types have been integrated in other programming language paradigms in addition to Haskell.
In particular, \citet{JML15,ScalasY16,PadFuse} integrate  \emph{binary} session types in the \emph{native} host language, without language extensions, to avoid hindering their use in practice.
To obtain this integration of session types, without extensions \citet{ScalasY16,PadFuse}) combine \emph{static} typing of input and output actions with \emph{run-time} checking of linear channel usage. These work implement session types in Scala and OCaml respectively, via a continuation-passing encoding of session types into linear types \citet{dardhagiachino12}.

Implementations of multiparty session types (MPST) are less common than binary implementations.  
\citet{Scalas2017} integrate MPST in Scala building upon \citet{ScalasY16} and a continuation-passing style encoding of session types into linear types.
\citet{SivaramakrishnanNZE10} implement MPST leveraging an extension of Java with session primitives. %
\citet{HY16} develops a MPST-based API generation for Java leveraging CFSMs \cite{Brand1983CFM} and \citet{KDPG16} implement session types in the form of \emph{typestates} in Java.
\citet{DHHNY2015} implement MPST in Python and \citet{Fowler16,NY2017} in Erlang, focusing on {purely dynamic} MPST verification via run-time monitoring.
\citet{NY2017A,NBY2017} extend
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
With regards to function languages, the original works on GV \cite{gayvasconcelos10,gayvasconcelos12} did not guarantee deadlock freedom. This was later addressed by \citet{lindleymorris15,wadler15} via syntactic restrictions where communication once again follows a tree structure. \citet{kokkedardha21} introduce PGV  following \citet{dardhagay18} allowing for more flexible programming in GV.

Other works of guaranteeing deadlock freedom in session-typed systems include the works by \citet{dezani-ciancaglinimostrous06}, where deadlock freedom is guaranteed by allowing only one active session at a time and and \citet{dezani-ciancagliniliguoro09progress}, where priorities \cite{kobayashi06} are used.
\citet{hondayoshida08} guarantee deadlock freedom \emph{within a single} session of MPST, but not for session interleaving.
\citet{kokke19} guarantees deadlock freedom of session types in Rust via enforcing a tree structure of communication actions.


\paragraph{Conclusion and future work}
