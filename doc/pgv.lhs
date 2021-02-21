\usingnamespace{sesh}
%include polycode.fmt
%format %              = "\empty"
%format 1              = "\empty"
%format ->             = "\multimap"
%format Sesh (p) (q) a = "\mon{" p "}{" q "}{" a "}"
%format Send o a s     = "\tysend[" o "]{" a "}{" s "}"
%format Recv o a s     = "\tyrecv[" o "]{" a "}{" s "}"
%format End  o         = "\tyend[" o "]"
%format Bot            = "\bot"
%format Top            = "\top"
%format Pr  a          = "\pr(\ty{" a "})"
%format Min p q        = p "\sqcap" q
%format Max p q        = p "\sqcup" q
%format a              = "\ty{a}"
%format b              = "\ty{b}"
%format s              = "\ty{s}"
%format p              = "\cs{p}"
%format p'             = "\cs{p'}"
%format p''            = "\cs{p''}"
%format q              = "\cs{q}"
%format q'             = "\cs{q'}"
%format q''            = "\cs{q''}"
%format return         = "\tm{\Varid{return}}"
%format >>=            = "\tm{\bind}"
%format join           = "\tm{\Varid{join}}"
%format new            = "\tm{\Varid{new}}"
%format spawn          = "\tm{\Varid{spawn}}"
%format send           = "\tm{\Varid{send}}"
%format recv           = "\tm{\Varid{recv}}"
%format close          = "\tm{\Varid{close}}"

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
