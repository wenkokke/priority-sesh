%include polycode.fmt
%include linear.fmt
%include main.fmt

\begin{spec}
class (Session (Dual s), Dual (Dual s) ~ s) => Session s where
  type Dual s = result | result -> s

  new :: Sesh (s, Dual)
\end{spec}
