\usingnamespace{sesh}
%include polycode.fmt
%include main.fmt

\input{fig/pgv-typing}

\input{fig/sesh-typing}

\begin{code}
return :: a %1 -> Sesh (Pr a) Bot a
(>>=) :: (q < p') => Sesh p q a %1 -> (a %1-> Sesh p' q' b) %1 -> Sesh (Min p p') (Max q q') b
join :: (q < p') => Sesh p q (Sesh p' q' a) %1 -> Sesh (Min p p') (Max q q') a
\end{code}

\begin{code}
new    :: () %1 -> Sesh Top Bot (s, Dual s)
spawn  :: (() %1 -> Sesh p q ()) %1 -> Sesh Top Bot ()
send   :: (a, Send o a s) %1  -> Sesh Top o s
recv   :: Recv o a s      %1  -> Sesh Top o (a, s)
close  :: End o           %1  -> Sesh Top o ()
\end{code}
