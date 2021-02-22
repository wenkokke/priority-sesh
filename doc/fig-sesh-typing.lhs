%include polycode.fmt
%include linear.fmt
%include main.fmt
\begin{figure}
\begin{mdframed}
\centering
\begin{code}
return  :: a %1 -> Sesh (Pr a) Bot a
(>>=)   ::  (q < p') =>
            Sesh p q a %1 -> (a %1-> Sesh p' q' b) %1 -> Sesh (Min p p') (Max q q') b
new     :: () %1 -> Sesh Top Bot (s, Dual s)
spawn   :: (() %1 -> Sesh p q ()) %1 -> Sesh Top Bot ()
send    :: (a, Send o a s) %1  -> Sesh Top o s
recv    :: Recv o a s      %1  -> Sesh Top o (a, s)
close   :: End o           %1  -> Sesh Top o ()
\end{code}
\caption{Typing rules for Sesh.}
\label{fig:sesh-typing}
\end{mdframed}
\end{figure}
