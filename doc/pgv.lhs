%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{Relation to Priority GV}
\label{sec:pgv}
The \texttt{priority-sesh} library is based on a variant of Priority GV~\cite{kokkedardha21}, which differs in three ways:
\begin{enumerate}
\item
  it marks lower bounds \emph{explicitly} on the sequent, rather than implicitly inferring them from the typing environment;
\item
  it collapses the isomorphic types for session end, $\pgv{\tyends[o]}$ and $\pgv{\tyendr[o]}$, into $\pgv{\tyend[o]}$;
\item
  it is extended with asynchronous communication and session cancellation following \citet{fowlerlindley19}.
\end{enumerate}
These changes preserve subject reduction and progress, and give us \emph{tighter} bounds on priorities. To see why, note that PCP and PGV use the \emph{smallest} priority in the typing environment as an approximation for the lower bound. Unfortunately, this \emph{underestimates} the lower bound in the rules \LabTirName{T-Var} and \LabTirName{T-Lam}. These rules type \emph{values}, which are pure and could have lower bound |Top|, but the smallest priority in their typing environment is not necessarily |Top|.

\paragraph{Priority GV}
We briefly revisit the syntax and type system of PGV, but a full discussion of PGV is out of scope for this paper. For a discussion of the synchronous semantics of PGV, and for the proofs of subject reduction, progress, and deadlock freedom, please see \citet{kokkedardha21}. For a discussion of the asynchronous semantics and session cancellation, please see \citet{fowlerlindley19}.
\[
  \usingnamespace{pgv}
  \begin{array}{lcl}
    \ty{S}
    & \Coloneqq & \ty{\tysend[\cs{o}]{T}{S}}
      \mid        \ty{\tyrecv[\cs{o}]{T}{S}}
      \mid        \ty{\tyend[\cs{o}]}
  \end{array}
\]
\[
  \usingnamespace{pgv}
  \begin{array}{lcl}
    \ty{T}, \ty{U}
    & \Coloneqq & \ty{\typrod{T}{U}}
      \mid        \ty{\tyunit}
      \mid        \ty{\tysum{T}{U}}
      \mid        \ty{\tyvoid}
      \mid        \ty{\tylolli{\cs{p}}{\cs{q}}{T}{U}}
      \mid        \ty{S}
  \end{array}
\]
\[
  \usingnamespace{pgv}
  \begin{array}{lcl}
    \tm{L}, \tm{M}, \tm{N}
    & \Coloneqq & \tm{x}
      \mid        \tm{K}
      \mid        \tm{\lambda x.M}
      \mid        \tm{M\;N} \\
    & \mid      & \tm{\unit}
      \mid        \tm{\andthen{M}{N}} \\
    & \mid      & \tm{\pair{M}{N}}
      \mid        \tm{\letpair{x}{y}{M}{N}} \\
    & \mid      & \tm{\absurd{M}} \\
    & \mid      & \tm{\inl{M}}
      \mid        \tm{\inr{M}}
      \mid        \tm{\casesum{L}{x}{M}{y}{N}}
    \\
    \tm{K}
    & \Coloneqq & \tm{\new}
      \mid        \tm{\fork}
      \mid        \tm{\send}
      \mid        \tm{\recv}
      \mid        \tm{\close}
  \end{array}
\]

\input{fig-pgv-typing}

\paragraph{Monadic Reflection}
We present the typing rules for our variant of PGV in \cref{fig:pgv-typing}. The graded monad |Sesh p q| arises from the monadic reflection~\cite{filinski94} of these typing rules. We translate types from PGV to \texttt{priority-sesh} as follows:
\[
  \setlength{\arraycolsep}{2pt}
  \begin{array}{lcl}
    \pgv{\tosesh{\ty{\tylolli{p}{q}{T}{U}}}}
    &=
    &|ToSesh T %1 -> Sesh p q (ToSesh U)|
    \\
    \pgv{\tosesh{\ty{\tysend[o]{T}{S}}}}
    &=
    &|Send o (ToSesh T) (ToSesh S)|
    \\
    \pgv{\tosesh{\ty{\tyrecv[o]{T}{S}}}}
    &=
    &|Recv o (ToSesh T) (ToSesh S)|
    \\
    \pgv{\tosesh{\ty{\tyend[o]}}}
    &=
    &|End o|
  \end{array}
  \;
  \begin{array}{lcl}
    \pgv{\tosesh{\ty{\tyunit}}}
    &=
    &|()|
    \\
    \pgv{\tosesh{\ty{\typrod{T}{U}}}}
    &=
    &|(ToSesh T, ToSesh U)|
    \\
    \pgv{\tosesh{\ty{\tyvoid}}}
    &=
    &|Void|
    \\
    \pgv{\tosesh{\ty{\tysum{T}{U}}}}
    &=
    &|Either (ToSesh T) (ToSesh U)|
  \end{array}
\]
%
We translate sequents from PGV to \texttt{priority-sesh} as:
\[
  \pgv{\tosesh{\seq{p}{q}{\Gamma}{M}{T}}}
  =
  |ToSesh Gamma|\vdash|tosesh M :: Sesh p q (ToSesh T)|
\]
%
%include fig-pgv-to-sesh-typing.lhs
%
We present a full translation from PGV programs to \texttt{priority-sesh} in \cref{fig:pgv-to-sesh-typing}. We translate the communication primitives from PGV to those with the same name in \texttt{priority-sesh}, module the unit arguments in $\pgv{\tm{\new}}$ and $\pgv{\tm{\fork}}$, which are necessary to create thunks in PGV as it's call-by-value:
\[
  \begin{array}{l}
    \pgv{\tosesh{\tmty{\new}{\tylolli{\top}{\bot}{\tyunit}{\typrod{S}{\co{S}}}}}}
    \\
    \quad=|\() -> new :: () %1 -> (ToSesh S, ToSesh (Dual S))|
    \\
    \pgv{\tosesh{\tmty{\fork}{\tylolli{\top}{\bot}{(\tylolli{p}{q}{\tyunit}{\tyunit})}{\tyunit}}}}
    \\
    \quad=|\k -> fork (k ()) :: (() %1 -> Sesh p q ()) %1 -> Sesh Top Bot ()|
  \end{array}
\]
