%include polycode.fmt
%include linear.fmt
%include main.fmt
\begin{figure}
\centering
\begin{align*}
  &\pgv{\tosesh{\ty{\tylolli{p}{q}{T}{U}}}}
  &&= |tosesh T %1 -> Sesh p q (tosesh U)|
  \\
  &\pgv{\tosesh{\ty{\tyunit}}}
  &&= |()|
  \\
  &\pgv{\tosesh{\ty{\typrod{T}{U}}}}
  &&= |(tosesh T, tosesh U)|
  \\
  &\pgv{\tosesh{\ty{\tysum{T}{U}}}}
  &&= |Either (tosesh T) (tosesh U)|
  \\
  &\pgv{\tosesh{\ty{\tyvoid}}}
  &&= |Void|
  \\
  &\pgv{\tosesh{\ty{\tysend[o]{T}{S}}}}
  &&= |Send o (tosesh T) (tosesh S)|
  \\
  &\pgv{\tosesh{\ty{\tyrecv[o]{T}{S}}}}
  &&= |Recv o (tosesh T) (tosesh S)|
  \\
  &\pgv{\tosesh{\ty{\tyend[o]}}}
  &&= |End o|
\end{align*}
\caption{Priority GV types to Sesh types.}
\label{fig:pgv-to-sesh-types}
\end{figure}
\begin{figure}
\[
\begin{array}{lcl}
  \pgv{\tosesh{{x}}}
  &=& |return x|
  \\
  \pgv{\tosesh{{\lambda{x}.L}}}
  &=& |return (\x -> tosesh L)|
  \\
  \pgv{\tosesh{{L\;M}}}
  &=& |tosesh L >>= \f -> M >>= \x -> f x|
  \\
  \pgv{\tosesh{{\unit}}}
  &=& |return ()|
  \\
  \pgv{\tosesh{{\letunit{L}{M}}}}
  &=& |tosesh L >>= \() -> M|
  \\
  \pgv{\tosesh{{\pair{L}{M}}}}
  &=& |tosesh L >>= \x -> tosesh M >>= \y -> return (x,y)|
  \\
  \pgv{\tosesh{{\letpair{x}{y}{L}{M}}}}
  &=& |tosesh L >>= \(x,y) -> M|
  \\
  \pgv{\tosesh{{\absurd{L}}}}
  &=& |tosesh L >>= \x -> absurd x|
  \\
  \pgv{\tosesh{{\inl{L}}}}
  &=& |tosesh L >>= \x -> return (Left x)|
  \\
  \pgv{\tosesh{{\inr{L}}}}
  &=& |tosesh L >>= \y -> return (Right y)|
  \\
  \multicolumn{3}{l}{%
  \pgv{\tosesh{{\casesum{L}{x}{M}{y}{N}}}} =}
  \\
  \multicolumn{3}{l}{%
  \qquad\qquad|tosesh L >>= \x -> case x { Left x -> tosesh M; Right y -> tosesh N }|}
\end{array}
\]
\caption{Priority GV terms to Sesh terms.}
\label{fig:pgv-to-sesh-terms}
\end{figure}
