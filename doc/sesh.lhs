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

Where |quiet| suppresses any |BlockedIndefinitelyOnMVar| errors.

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

\begin{spec}
connect ::  Session s => (s %1 -> Linear.IO ()) %1 ->
            (Dual s %1 -> Linear.IO a) %1 -> Linear.IO a
connect k1 k2 = do (s1, s2) <- new; spawn (k1 s1); k2 s2
\end{spec}


\subsection{Session-typed channels with priority}\label{sec:priority-sesh}

Priorities are either |Bot|, a~natural number, or |Top|. A~natural number priority represents the time at which some action happens---the lower the number, the sooner it happens. The values |Top| and |Bot| are used as the identities for |`Min`| and |`Max`| in lower and upper bounds on priorities, respectively. We let |o| range over natural numbers, |p| over \emph{lower bounds}, and |q| over \emph{upper bounds}.

\begin{spec}
data Priority = Bot | Val Nat | Top
\end{spec}

We define strict inequality ($|`LT`|$), minimum (|`Min`|), and maximum (|`Max`|) on priorities as usual.

We define |Send o|, |Recv o|, and |End o|, which decorate the raw sessions from~\cref{sec:unsafe-sesh} with the priority |o| of the communication action, \ie, when does the communication happen? Duality (|Dual|) preserves these priorities. Operationally, these types are mere wrappers.

We define a graded monad |Sesh p q|, which decorates |Linear.IO| with a lower bound |p| and an upper bound |q| on the priorities of its communication actions, \ie, if you run the monad, when does communication begin and end?

\begin{spec}
newtype Sesh p q a = MkSesh { runSeshIO :: Linear.IO a }
\end{spec}

The monad operations for |Sesh p q| merely wrap those for |Linear.IO|, hence trivially obeys the monad laws.

The |ireturn| function returns a \emph{pure} computation---the type |Sesh Top Bot| guarantees that all communications happen between |Top| and |Bot|, hence there can be no communication at all!

\begin{spec}
ireturn :: a %1 -> Sesh Top Bot a
ireturn x = MkSesh $ return x
\end{spec}

The |>>>=| operator sequences two actions with types |Sesh p q| and |Sesh p' q'|, and requires |LT q p'|, \ie, the first action must have finished before the second starts. The resulting action has lower bound |Min p p'| and upper bound |Max q q'|.

\begin{spec}
(>>>=) :: (LT q p') => Sesh p q a %1 -> (a %1-> Sesh p' q' b) %1 -> Sesh (Min p p') (Max q q') b
mx >>>= mf = MkSesh $ runSeshIO mx >>= \x -> runSeshIO (mf x)
\end{spec}

We define similar wrappers for the concurrency and communication primitives:
\begin{itemize*}[label=\empty]
\item |send|, |recv|, and |close| each perform a communication action with some priority |o|, and return a computation of type |Sesh o o|, \ie, with \emph{exact} bounds;
\item |new|, |spawn|, and |cancel| don't perform any communication action, and so return a \emph{pure} computation of type |Sesh Top Bot|.
\end{itemize*}

\begin{spec}
send     :: Session s => (a, Send o a s) %1 -> Sesh (Val o) (Val o) s
recv     :: Session s => Recv o a s %1 -> Sesh (Val o) (Val o) (a, s)
close    :: End o %1 -> Sesh (Val o) (Val o) ()
new      :: Session s => Sesh Top Bot (s, Dual s)
spawn    :: Sesh p q () %1 -> Sesh Top Bot ()
cancel   :: Session s => s %1 -> Sesh Top Bot ()
\end{spec}


