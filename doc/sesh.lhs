%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{What is Priority Sesh?}

\subsection{One-shot channels}

\begin{spec}
type SendOnce  a = MVar a
type RecvOnce  a = MVar a
\end{spec}

\begin{spec}
newOneShot :: Linear.IO (SendOnce a, RecvOnce a)
newOneShot = bimap unur unur . dup2 <$> newEmptyMVar

sendOneShot :: SendOnce a %1 -> a %1 -> Linear.IO (Ur ())
sendOneShot mvar x = do putMVar mvar x; return $ Ur ()

recvOneShot :: RecvOnce a %1 -> Linear.IO a
recvOneShot mvar = takeMVar mvar
\end{spec}

\subsection{Session-typed channels}

\begin{spec}
class (Session (Dual s) , Dual (Dual s) ~ s) => Session s
  where
    type Dual s = result | result -> s
    new :: RawSesh (s, Dual s) -- unsafe, not exported
\end{spec}

\begin{spec}
data RawSend a s  = (Session s) => MkRawSend (SendOnce (a, Dual s))
data RawRecv a s  = (Session s) => MkRawRecv (RecvOnce (a, s))
data RawEnd       = MkRawEnd (SendOnce (), RecvOnce ())
\end{spec}

\begin{spec}
instance Session s => Session (RawSend a s)
  where
    type Dual (RawSend a s) = RawRecv a (Dual s)
    new :: Linear.IO (RawSend a s, RawRecv a (Dual s))
    new = bimap MkRawSend MkRawRecv <$> newOneShot

instance Session s => Session (RawRecv a s)
  where
    type Dual (RawRecv a s) = RawSend a (Dual s)
    new :: Linear.IO (RawRecv a s, RawSend a (Dual s))
    new = bimap MkRawRecv MkRawSend <$> newOneShot

instance Session End
  where
    type Dual End = End
    new :: Linear.IO (End, End)
    new = do  (s1, r1) <- newOneShot
              (s2, r2) <- newOneShot
              return (MkRawEnd s1 r2, MkRawEnd s2 r2)
\end{spec}

\subsection{Session-typed channels with priority}

\begin{spec}
withNew :: Session s => ((s, Dual s) %1 -> RawSesh a) %1 -> RawSesh a
withNew k = new >>= k
\end{spec}

\begin{spec}
data RawSend a s  = MkRawSend  (Sender (a, Dual s)))
data RawRecv a s  = MkRawRecv  (Receiver (a, s))
data RawEnd       = MkRawEnd   (Sender (), Receiver ())
\end{spec}
