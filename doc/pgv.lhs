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
We briefly revisit the syntax and type system of PGV, but a full discussion of PGV is out of scope for this paper. For a discussion of the \emph{synchronous} semantics for PGV, and the proofs of subject reduction, progress, and deadlock freedom, please see \citet{kokkedardha21}. For a discussion of the \emph{asynchronous} semantics and session cancellation, please see \citet{fowlerlindley19}.

As in \cref{sec:priority-sesh}, we let $\cs{o}$ range over priorities, which are natural numbers, and $\cs{p}$ and $\cs{q}$ over priority bounds, which are either natural numbers, $\cs{\top}$, or $\cs{\bot}$.

PGV is based on the standard linear $\lambda$-calculus with product types ($\pgv{\ty{\typrod{\cdot}{\cdot}}}$), sum types ($\pgv{\ty{\tysum{\cdot}{\cdot}}}$), and their units ($\pgv{\ty{\tyunit}}$ and $\pgv{\ty{\tyvoid}}$). Linear functions ($\pgv{\ty{\tylolli{p}{q}{\cdot}{\cdot}}}$) are annotated with priority bounds which tell us, if the function is applied, when communication begins and ends.

Types and session types are defined as follows:
\[
  \usingnamespace{pgv}
  \begin{array}{lcl}
    \ty{S}
    & \Coloneqq & \ty{\tysend[\cs{o}]{T}{S}}
      \mid        \ty{\tyrecv[\cs{o}]{T}{S}}
      \mid        \ty{\tyend[\cs{o}]}
    \\
    \ty{T}, \ty{U}
    & \Coloneqq & \ty{\typrod{T}{U}}
      \mid        \ty{\tyunit}
      \mid        \ty{\tysum{T}{U}}
      \mid        \ty{\tyvoid}
      \mid        \ty{\tylolli{\cs{p}}{\cs{q}}{T}{U}}
      \mid        \ty{S}
  \end{array}
\]
The types $\pgv{\ty{\tysend[\cs{o}]{T}{S}}}$ and $\pgv{\ty{\tyrecv[\cs{o}]{T}{S}}}$ mean ``send'' and ``receive'', respectively, and $\pgv{\ty{\tyend[\cs{o}]}}$ means, well, session end.

The term language is the standard linear $\lambda$-calculus extended with concurrency primitives $\pgv{\tm{K}}$:
\[
  \usingnamespace{pgv}
  \begin{array}{lcl}
    \multicolumn{3}{l}{\tm{L}, \tm{M}, \tm{N}}
    \\
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
The concurrency primitives are uninterpreted in the term language. Rather, they are interpreted in a configuration language based on the $\pi$-calculus, which we omit from this paper (see \citet{kokkedardha21}).

\input{fig-pgv-typing}

We present the typing rules for PGV in \cref{fig:pgv-typing}. A sequent $\pgv{\seq{p}{q}{\ty{\Gamma}}{M}{T}}$ should be read as ``$\pgv{\tm{M}}$ is well-typed PGV program with type $\pgv{\ty{T}}$ in typing environment $\pgv{\ty{\Gamma}}$, and when run it starts communicating at time $\pgv{\cs{p}}$ and stops at time $\pgv{\cs{q}}$.''

\paragraph{Monadic Reflection}
The graded monad |Sesh p q| arises from the \emph{monadic reflection}~\cite{filinski94} of the typing rules in \cref{fig:pgv-typing}. Monadic reflection is a technique for translating programs in an effectful language to \emph{monadic} programs in a pure language. For instance, \citet{filinski94} demonstrates the reflection from programs of type $\pgv{\ty{T}}$ in a language with exceptions and handlers to programs of type $\pgv{\ty{\tysum{T}{\mathbf{exn}}}}$ in a pure language where $\pgv{\ty{\mathbf{exn}}}$ is the type of exceptions.

We translate programs from PGV to Haskell programs in the |Sesh p q| monad. We start by translating types as follows:
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
We translate programs in PGV to Haskell programs in the |Sesh p q| monad. We using $\Gamma\vdash|x :: a|$ to mean that the Haskell program |x| has type |a| in typing environment |Gamma|, even though typing environments aren't generally explicit in Haskell:
\[
  \pgv{\tosesh{\seq{p}{q}{\Gamma}{M}{T}}}
  =
  |ToSesh Gamma|\vdash|tosesh M :: Sesh p q (ToSesh T)|
\]
%
%include fig-pgv-to-sesh-typing.lhs
%
We present a full translation from PGV programs to \texttt{priority-sesh} in \cref{fig:pgv-to-sesh-typing}. We translate the communication primitives from PGV to those with the same name in \texttt{priority-sesh}, with some minor changes in the translations of $\pgv{\tm{\new}}$ and $\pgv{\tm{\fork}}$, where PGV needs some unit arguments to create thunks in PGV, as it's call-by-value, which aren't needed in Haskell, which is call-by-need:
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
