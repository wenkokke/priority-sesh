%include polycode.fmt
%include linear.fmt
%include main.fmt
\begin{figure*}
  \centering
  \begin{align*}
    &\pgv{\tosesh{\tm{x}}}
    &&= |return x|
    \\
    &\pgv{\tosesh{\tm{\lambda{x}.L}}}
    &&= |return (\x -> tosesh L)|
    \\
    &\pgv{\tosesh{\tm{L\;M}}}
    &&= |tosesh L >>= \f -> M >>= \x -> f x|
    \\
    &\pgv{\tosesh{\tm{\unit}}}
    &&= |return ()|
    \\
    &\pgv{\tosesh{\tm{\letunit{L}{M}}}}
    &&= |tosesh L >>= \() -> M|
    \\
    &\pgv{\tosesh{\tm{\pair{L}{M}}}}
    &&= |tosesh L >>= \x -> tosesh M >>= \y -> return (x,y)|
    \\
    &\pgv{\tosesh{\tm{\letpair{x}{y}{L}{M}}}}
    &&= |tosesh L >>= \(x,y) -> M|
    \\
    &\pgv{\tosesh{\tm{\inl{L}}}}
    &&= |tosesh L >>= \x -> Left x|
    \\
    &\pgv{\tosesh{\tm{\inr{L}}}}
    &&= |tosesh L >>= \y -> Right y|
    \\
    &\pgv{\tosesh{\tm{\casesum{L}{x}{M}{y}{N}}}}
    &&= |tosesh L >>= \x -> case x { Left x -> tosesh M; Right y -> tosesh N }|
    \\
    &\pgv{\tosesh{\tm{\absurd{M}}}}
    &&= |tosesh L >>= \x -> absurd x|
  \end{align*}
  \caption{Translation from Priority GV to Sesh.}
  \label{fig:pgv-to-sesh}
\end{figure*}
