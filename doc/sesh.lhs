%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{What is Priority Sesh?}\label{sec:sesh}

We colour the Haskell definitions which are a part of Sesh:
\begin{itemize*}[font=\bfseries]
\item[\tm{red}] for functions and constructors;
\item[\ty{blue}] for types and type families; and
\item[\cs{emerald}] for priorities and type families acting on priorities.
\end{itemize*}

\subsection{One-shot channels}\label{sec:one-shot}

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


\subsection{Session-typed channels}\label{sec:unsafe-sesh}

\begin{spec}
class (Session (Dual s) , Dual (Dual s) ~ s) => Session s
  where
    type Dual s = result | result -> s
    new :: Linear.IO (s, Dual s)
\end{spec}

We define |spawn|, which spawns off a new thread, as essentially an alias of |forkLinearIO|.

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

cancel :: Session s => s %1 -> Linear.IO ()
cancel s = return $ consume s
\end{spec}

Where |quiet| suppresses any |BlockedIndefinitelyOnMVar| errors.

\begin{spec}
connect ::  Session s => (s %1 -> Linear.IO ()) %1 ->
            (Dual s %1 -> Linear.IO a) %1 -> Linear.IO a
connect k1 k2 = do (s1, s2) <- new; spawn (k1 s1); k2 s2
\end{spec}


\subsection{Session-typed channels with priority}\label{sec:priority-sesh}

Priorities are either |Bot|, a~natural number, or |Top|. We let |p| and |q| range over \emph{all} priorities, and |o| over natural numbers. A~priority |o| represents the time at which some action happens. Actions with lower priorities happen before actions with higher priorities. Actions always have natural number priorities. The values |Top| and |Bot| are used as the neutral elements in lower and upper priority bounds, respectively.

\begin{spec}
data Priority = Bot | Val Nat | Top
\end{spec}

We define |Send o|, |Recv o|, and |End o|, which decorate the raw sessions from~\cref{sec:unsafe-sesh} with the priority |o| of the communication, \ie, when does the communication happen? Operationally, these types are mere wrappers.

We define the |Sesh p q| monad, which decorates |Linear.IO| with a lower bound |p| and an upper bound |q| on the priorities its communications, \ie, if you run the monad, when does communication begin and end?

\begin{spec}
newtype Sesh p q a = MkSesh { runSeshIO :: Linear.IO a }
\end{spec}

We define strict inequality ($|<|$), minimum (|`Min`|), and maximum (|`Max`|) on priorities as usual, and define an open type family (|Pr|) which returns a \emph{lower bound} on the priority for any type:

\begin{spec}
type family Pr (a :: Type) :: Priority
type instance Pr (Sesh p q a)  = p
type instance Pr (Send o a s)  = Val o
type instance Pr (Recv o a s)  = Val o
type instance Pr (End o)       = Val o
type instance Pr ()            = Top
type instance Pr (a -> b)      = Pr b
type instance Pr (Either a b)  = Min (Pr a) (Pr b)
type instance Pr (a, b)        = Min (Pr a) (Pr b)
{-"\dots"-}
\end{spec}

The monad operations for |Sesh p q| merely wrap those for |Linear.IO|.

\begin{spec}
ireturn :: a %1 -> Sesh (Pr a) Bot a
ireturn x = MkSesh $ return x

(>>>=) :: (q < p') => Sesh p q a %1 -> (a %1-> Sesh p' q' b) %1 -> Sesh (Min p p') (Max q q') b
mx >>>= mf = MkSesh $ runSeshIO mx >>= \x -> runSeshIO (mf x)
\end{spec}


\begin{spec}
new      :: Session s => Sesh Top Bot (s, Dual s)
spawn    :: Sesh p q () %1 -> Sesh Top Bot ()
send     :: Session s => (a, Send o a s) %1 -> Sesh (Val o) (Val o) s
recv     :: Session s => Recv o a s %1 -> Sesh (Val o) (Val o) (a, s)
close    :: End o %1 -> Sesh (Val o) (Val o) ()
cancel   :: Session s => s %1 -> Sesh Top Bot ()
\end{spec}


