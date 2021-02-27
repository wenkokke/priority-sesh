%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{What is Priority Sesh?}\label{sec:main}

We introduce Priority Sesh in three steps: in~\cref{sec:one-shot}, we build a small library of \emph{linear} or \emph{one-shot channels} based on MVars~\cite{peytonjonesgordon96}; in~\cref{sec:sesh}, we use these one-shot channels to build a small library of \emph{session-typed channels} \cite{dardhagiachino12}; and in~\cref{sec:priority-sesh}, we decorate these session types with \emph{priorities} to guarantee deadlock-freedom \cite{kokkedardha21}.

Priority Sesh is written in Linear Haskell~\cite{bernardyboespflug18}. The type |%1 ->| is syntactic sugar for the linear arrow @%1->@. Familiar definitions refer to linear variants packaged with \texttt{linear-base}\footnote{\url{https://github.com/tweag/linear-base/}} (\eg, |Functor|, |Bifunctor|, |Monad|) or with Priority Sesh (\eg, |MVar|). For clarity, we refer to the linear |IO| monad from \texttt{linear-base} as |Linear.IO|.

We colour the Haskell definitions which are a part of Sesh:
\begin{itemize*}[font=\bfseries]
\item[\tm{red}] for functions and constructors;
\item[\ty{blue}] for types and type families; and
\item[\cs{emerald}] for priorities and type families acting on priorities.
\end{itemize*}

\subsection{One-shot channels}\label{sec:one-shot}

We start by building a small library of \emph{linear} or \emph{one-shot channels}, \ie, channels over which a value must be sent or received \emph{exactly once}.

The one-shot channels are at the core of our library, and their efficiency is crucial to the overall efficiency of Priority Sesh. However, we do not aim to present an efficient implementation here. Rather, we aim to present a compact implementation with the correct behaviour.


\paragraph{Channels}
A~one-shot channel has two endpoints, |SendOnce| and |RecvOnce|, which are two copies of the same |MVar|.
\begin{spec}
newtype SendOnce  a = MkSendOnce (MVar a)
newtype RecvOnce  a = MkRecvOnce (MVar a)

newOneShot :: Linear.IO (SendOnce a, RecvOnce a)
newOneShot = do  (mvar_s, mvar_r) <- dup2 <$> newEmptyMVar
                 return (MkSendOnce (unur mvar_s), MkRecvOnce (unur mvar_r))
\end{spec}
The |newEmptyMVar| function returns an \emph{unrestricted} |MVar|, which may be used non-linearly. The |dup2| function creates two (unrestricted) copies of the |MVar|. The |unur| function casts each \emph{unrestricted} copy to a \emph{linear} copy. Thus, we end up with two copies of an |MVar|, each of which must be used \emph{exactly once}.

We implement |sendOnce| and |recvOnce| as aliases for the corresponding |MVar| operations.
\begin{spec}
sendOnce :: SendOnce a %1 -> a %1 -> Linear.IO ()
sendOnce (MkSendOnce mvar_s) x = putMVar mvar_s x

recvOnce :: RecvOnce a %1 -> Linear.IO a
recvOnce (MkRecvOnce mvar_r)= takeMVar mvar_r
\end{spec}
The |MVar| operations implement the correct blocking behaviour for asynchronous one-shot channels: the |sendOnce| operation is non-blocking, and the |recvOnce| operations blocks until a value becomes available.


\paragraph{Synchronisation}
We use |SendOnce| and |RecvOnce| to implement a construct for one-shot synchronisation between two processes, |SyncOnce|, which consists of two one-shot channels. To synchronise, each process sends a unit on the one channel, then waits to receive a unit on the other channel.
\begin{spec}
data SyncOnce = MkSyncOnce (SendOnce ()) (RecvOnce ())

newSync :: Linear.IO (SyncOnce, SyncOnce)
newSync = do  (ch_s1, ch_r1) <- newOneShot
              (ch_s2, ch_r2) <- newOneShot
              return (MkSyncOnce ch_s1 ch_r2, MkSyncOnce ch_s2 ch_r1)

syncOnce :: SyncOnce %1 -> Linear.IO ()
syncOnce (MkSyncOnce ch_s ch_r) = do sendOnce ch_s (); recvOnce ch_r
\end{spec}


\paragraph{Cancellation}
One-shot channels are created in the linear |IO| monad, so \emph{forgetting} to use a channel results in a complaint from the type-checker. However, it is possible to \emph{explicitly} drop values whose types implement the |Consumable| class, using |consume :: a %1 -> ()|.

One-shot channels implement |Consumable| by simply dropping their |MVar|s. The Haskell runtime throws an exception when a ``thread is blocked on an |MVar|, but there are no other references to the |MVar| so it can't ever continue.''\footnote{\url{https://downloads.haskell.org/~ghc/9.0.1/docs/html/libraries/base-4.15.0.0/Control-Exception.html\#t:BlockedIndefinitelyOnMVar}} Practically, |consumeSend| throws a |BlockedIndefinitelyOnMVar| exception, whereas |consumeRecv| does not.
\begin{spec}
consumeSend = do  (ch_s, ch_r) <- newOneShot
                  spawn $ return (consume ch_s)
                  recvOnce ch_r

consumeRecv = do  (ch_s, ch_r) <- newOneShot
                  spawn $ return (consume ch_r)
                  sendOnce ch_s ()
\end{spec}
Where |spawn| spawns off a new thread using a linear |forkIO|.


\subsection{Session-typed channels}\label{sec:sesh}
Let's look at a simple example of a session-typed channel---a multiplication service.
\begin{spec}
type MulServer = RawRecv Int (RawRecv Int (RawSend Int RawEnd))
\end{spec}

\begin{center}
\begin{minipage}{0.475\linewidth}
\begin{spec}
mulServer (s :: MulServer)
  = do  (x, s) <- recv s
        (y, s) <- recv s
        s <- send (x * y, s)
        close s
        return ()
\end{spec}
\end{minipage}%
\begin{minipage}{0.525\linewidth}
\begin{spec}
mulClient (s :: Dual MulServer)
  = do  s <- send (32, s)
        s <- send (41, s)
        (z, s) <- recv s
        close s
        return z
\end{spec}
\end{minipage}%
\end{center}
We use the one-shot channels to build a small library of \emph{session-typed channels}.

First, we define the |Session| class. A~session type must have a |Dual|, which must itself be a session type. Duality must be an \emph{injective} and \emph{involutive} function. Finally, |new| creates a session-typed channel.
\begin{spec}
class (Session (Dual s) , Dual (Dual s) ~ s) => Session s
  where
    type Dual s = result | result -> s
    new :: Linear.IO (s, Dual s)
\end{spec}
There are three primitive session types: |RawSend|, |RawRecv|, and |RawEnd|.
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
    new = bimap MkRawEnd MkRawEnd <$> newSync
\end{spec}

\begin{spec}
send :: (a, RawSend a s) %1 -> Linear.IO s
send (x, MkRawSend ch_s) = do
  (here, there) <- new
  sendOnce ch_s (x, there)
  return here

recv :: RawRecv a s %1 -> Linear.IO (a, s)
recv (MkRawRecv ch_r) = recvOnce ch_r

close :: RawEnd %1 -> Linear.IO ()
close (MkRawEnd sync) = syncOnce sync
\end{spec}

TODO: |cancel| calls |consume|, but channels do not implement consumable to avoid implicit dropping of channels due to desugaring of do-notation

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

We define |Send o|, |Recv o|, and |End o|, which decorate the raw sessions from~\cref{sec:sesh} with the priority |o| of the communication action, \ie, when does the communication happen? Duality (|Dual|) preserves these priorities. Operationally, these types are mere wrappers.

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

