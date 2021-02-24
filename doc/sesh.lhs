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

sendOnce :: SendOnce a %1 -> a %1 -> Linear.IO ()
sendOnce mvar x = putMVar mvar x

recvOnce :: RecvOnce a %1 -> Linear.IO a
recvOnce mvar = takeMVar mvar
\end{spec}

\begin{spec}
type SyncOnce = (SendOnce (), RecvOnce ())

newSync :: Linear.IO (SyncOnce, SyncOnce)
newSync = do  (mvar_s1, mvar_r1) <- newOneShot
              (mvar_s2, mvar_r2) <- newOneShot
              return ((mvar_s1, mvar_r2), (mvar_s2, mvar_r1))

syncOnce :: SyncOnce %1 -> Linear.IO ()
syncOnce (mvar_s, mvar_r) = do sendOnce mvar_s (); recvOnce mvar_r
\end{spec}


\subsection{Session-typed channels}

\begin{spec}
class (Session (Dual s) , Dual (Dual s) ~ s) => Session s
  where
    type Dual s = result | result -> s
    new :: Linear.IO (s, Dual s) -- unsafe, not exported
\end{spec}

\begin{spec}
data RawSend a s  = (Session s) => MkRawSend (SendOnce (a, Dual s))
data RawRecv a s  = (Session s) => MkRawRecv (RecvOnce (a, s))
data RawEnd       = MkRawEnd SyncOnce
\end{spec}

\begin{spec}
instance Session s => Session (RawSend a s)
  where
    type Dual (RawSend a s) = RawRecv a (Dual s)
    new = bimap MkRawSend MkRawRecv <$> newOneShot

instance Session s => Session (RawRecv a s)
  where
    type Dual (RawRecv a s) = RawSend a (Dual s)
    new = bimap MkRawRecv MkRawSend . swap <$> newOneShot

instance Session RawEnd
  where
    type Dual RawEnd = RawEnd
    new = MkRawEnd <$> newSync
\end{spec}

\begin{spec}
send :: (a, RawSend a s) %1 -> Linear.IO s
send (x, MkRawSend mvar_s) = do
  (here, there) <- new
  () <- quiet $ sendOnce mvar_s (x, there)
  return here

recv :: RawRecv a s %1 -> Linear.IO (a, s)
recv (MkRawRecv mvar_r) = recvOnce mvar_r

close :: RawEnd %1 -> Linear.IO ()
close (MkRawEnd sync) = quiet $ syncOnce sync
\end{spec}

Where |quiet| suppresses any |BlockedIndefinitelyOnMVar| errors.

\begin{spec}
withNew ::  Session s =>
            ((s, Dual s) %1 -> Linear.IO a) %1 -> Linear.IO a
withNew k = new >>= k
\end{spec}

\subsection{Session-typed channels with priority}

\begin{spec}
data Priority = Bot | Val Nat | Top
\end{spec}

We define |<|, |Min|, and |Max| on priorities as expected.

\begin{spec}
newtype Sesh p q a = MkSesh { runSesh :: Linear.IO a }

ireturn :: a %1 -> Sesh (Pr a) Bot a
ireturn x = MkSesh $ return x

(>>>=) :: (q < p') => Sesh p q a %1 -> (a %1-> Sesh p' q' b) %1 -> Sesh (Min p p') (Max q q') b
mx >>>= f = MkSesh $ runSesh mx >>= \x -> runSesh (f x)
\end{spec}

\begin{spec}
data Send o  a s  = Session s => MkSend (Raw.RawSend a (Raw s))
data Recv o  a s  = Session s => MkRecv (Raw.RawRecv a (Raw s))
data End o        = MkEnd Raw.RawEnd
\end{spec}

\begin{spec}
new :: Session s => Sesh Top Bot (s, Dual s)
new = Sesh $ bimap fromRaw fromRaw <$> Raw.new
\end{spec}
