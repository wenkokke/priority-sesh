\section{Conclusion, related and future work}

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

\paragraph{Session types and deadlock freedom}

\paragraph{Conclusion and future work}
