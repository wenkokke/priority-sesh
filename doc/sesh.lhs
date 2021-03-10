%include polycode.fmt
%include linear.fmt
%include main.fmt

\section{What is Priority Sesh?}\label{sec:main}

In this section we introduce Priority Sesh in three steps:
\begin{enumerate}
\item
in~\cref{sec:one-shot}, we build a small library of \emph{linear} or \emph{one-shot channels} based on MVars~\cite{peytonjonesgordon96};
\item
in~\cref{sec:sesh}, we use these one-shot channels to build a small library of \emph{session-typed channels} \cite{dardhagiachino12}; and
\item
in~\cref{sec:priority-sesh}, we decorate these session types with \emph{priorities} to guarantee deadlock-freedom \cite{kokkedardha21}.
\end{enumerate}

It is important to notice that the meaning of linearity in \emph{one-shot channels} differs from linearity in \emph{session channels}. A linear or one-shot channel comes from the linear $\pi$-calculus \cite{KPT99} where a channel must be used \emph{exactly once in input or output}; whether linearity in session types means that a session channel is used \emph{exactly once by a participant communicating in parallel} but the channel itself is used multiple times is sequence, by following the structure of the declared session type.

Priority Sesh is written in Linear Haskell~\cite{bernardyboespflug18}. The type |%1 ->| is syntactic sugar for the linear arrow @%1->@. Familiar definitions refer to linear variants packaged with \texttt{linear-base}\footnote{\url{https://github.com/tweag/linear-base/}} (\eg, |Functor|, |Bifunctor|, |Monad|) or with Priority Sesh (\eg, |MVar|). For clarity, we refer to the linear |IO| monad from \texttt{linear-base} as |Linear.IO|.

We colour the Haskell definitions which are a part of Sesh:
\begin{itemize*}[font=\bfseries]
\item[\tm{red}] for functions and constructors;
\item[\ty{blue}] for types and type families; and
\item[\cs{emerald}] for priorities and type families acting on priorities.
\end{itemize*}

\subsection{Library of one-shot channels}\label{sec:one-shot}

We start by building a small library of \emph{linear} or \emph{one-shot channels}, \ie, channels that must be use \emph{exactly once} to send or receive a value.

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

One-shot channels implement |Consumable| by simply dropping their |MVar|s. The Haskell runtime throws an exception when a ``thread is blocked on an |MVar|, but there are no other references to the |MVar| so it can't ever continue.''\footnote{\url{https://downloads.haskell.org/~ghc/9.0.1/docs/html/libraries/base-4.15.0.0/Control-Exception.html\#t:BlockedIndefinitelyOnMVar}} Practically, |consumeAndRecv| throws a |BlockedIndefinitelyOnMVar| exception, whereas |consumeAndSend| does not:
\begin{center}
\begin{minipage}{0.5\linewidth}
\begin{spec}
consumeAndRecv = do
  (ch_s, ch_r) <- newOneShot
  fork $ return (consume ch_s)
  recvOnce ch_r
\end{spec}
\end{minipage}%
\begin{minipage}{0.5\linewidth}
\begin{spec}
consumeAndSend = do u
  (ch_s, ch_r) <- newOneShot
  fork $ return (consume ch_r)
  sendOnce ch_s ()
\end{spec}
\end{minipage}%
\end{center}
Where |fork| forks off a new thread using a linear |forkIO|.

As the |BlockedIndefinitelyOnMVar| check is performed by the runtime, it'll even happen when a channel is dropped for reasons other than consume, such as a process crashing.


\subsection{Library of session-typed channels}\label{sec:sesh}
We now use the one-shot channels to build a small library of \emph{session-typed channels}.

\paragraph{An example}
Let's look at a simple example of a session-typed channel---a multiplication service, which receives two integers, sends back their product, and then terminates:
\begin{spec}
type MulServer = RawRecv Int (RawRecv Int (RawSend Int RawEnd))
type MulClient = RawSend Int (RawSend Int (RawRecv Int RawEnd))
\end{spec}
We define |mulServer|, which acts on a channel of type |MulServer|, and |mulClient|, which acts on a channel of the \emph{dual} type:
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
mulClient (s :: MulClient)
  = do  s <- send (32, s)
        s <- send (41, s)
        (z, s) <- recv s
        close s
        return z
\end{spec}
\end{minipage}%
\end{center}
Each action on a session-typed channel returns a channel for the \emph{continuation} of the session---save for |close|, which ends the session. Furthermore, |mulServer| and |mulClient| act on endpoints with \emph{dual} types. \emph{Duality} is crucial to session types as it ensures that when one process sends, the other is ready to receive, and vice versa. This is the basis for communication safety guaranteed by a session type system.

\paragraph{Channels}
We start by formalising this notion of duality. Each session type must have a dual, which must itself be a session type. Duality must be an \emph{injective} and \emph{involutive} function. These constraints are all captured by the |Session| class, along with |new| for constructing channels:
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
A channel |RawSend| wraps a one-shot channel |SendOnce| over which we send some value---which is the intended value sent by the session channel, and the channel over which \emph{the communicating partner process} continues the session---it'll make more sense once you read the definition for |send|.
A channel |RawRecv| wraps a one-shot channel |RecvOnce| over which we receive some value and the channel over which \emph{we} continue the session.
Finally, an channel |RawEnd| wraps a synchronisation.

We define duality for each session type---|RawSend| is dual to |RawRecv|, |RawRecv| is dual to |RawSend|, and |RawEnd| is dual to itself:
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
The |send| operation constructs a channel for the continuation of the session, then sends one endpoint of that channel, along with the value, over its one-shot channel, and returns the other endpoint:
\begin{spec}
send :: (a, RawSend a s) %1 -> Linear.IO s
send (x, MkRawSend ch_s) = do  (here, there) <- new
                               sendOnce ch_s (x, there)
                               return here
\end{spec}
The |recv| and |close| operations simply wrap their corresponding one-shot operations:
\begin{spec}
recv :: RawRecv a s %1 -> Linear.IO (a, s)
recv (MkRawRecv ch_r) = recvOnce ch_r

close :: RawEnd %1 -> Linear.IO ()
close (MkRawEnd sync) = syncOnce sync
\end{spec}

\paragraph{Asynchronous close}
We don't always \emph{want} session-end to involve synchronisation. Unfortunately, the |close| operation is synchronous.

An advantage of defining session types via a type class is that its an \emph{open} class, and we can add new primitives whenever! Let's make the unit type, |()|, a session type:
\begin{spec}
instance Session s => Session ()
  where
    type Dual () = ()
    new = return ((), ())
\end{spec}
Units are naturally affine---they contain \emph{zero} information, so dropping them won't harm---and the linear |Monad| class allows you to silently drop unit results of monadic computations.
They're ideal for \emph{asynchronous} session end!

\paragraph{Cancellation}
We implement session \emph{cancellation} via the |Consumable| class.  For convenience, we provide the |cancel| function:
\begin{spec}
	cancel :: Session s => s %1 -> Linear.IO
	cancel = return . consume
\end{spec}
As with one-shot channels, we |consume| simply drops the channel, and relies on the |BlockedIndefinitelyOnMVar| check, which means that |cancelAndRecv| throws an exception and |cancelAndSend| does not:
\begin{center}
\begin{minipage}{0.5\linewidth}
\begin{spec}
cancelAndRecv = do
  (ch_s, ch_r) <- new
  fork $ cancel ch_s
  ((), ()) <- recv ch_r
  return ()
\end{spec}
\end{minipage}%
\begin{minipage}{0.5\linewidth}
\begin{spec}
cancelAndSend = do u
  (ch_s, ch_r) <- new
  fork $ cancel ch_r
  () <- send ch_s ()
  return ()
\end{spec}
\end{minipage}%
\end{center}
These semantics correspond to EGV~\cite{fowlerlindley19}.

\paragraph{Choice}

\todo{%
	Choice can be encoded as below. This is a \emph{different} form of extension from |()|. It's built on top of the primitives, so we don't even have to fuss about giving |Select| and |Offer|, nor do we have to worry about one-shot channels!}

\begin{spec}
type Select  s1 s2 = RawSend (Either (Dual s1) (Dual s2)) ()
type Offer  s1 s2 = RawRecv (Either s1 s2) ()

selectLeft :: (Session s1, Session s2) =>
  Select s1 s2 %1 -> Linear.IO s1
selectLeft s = do  (here, there) <- new
                   send (Left there, s)
                   return here

offerEither :: (Session s1, Session s2) => Offer s1 s2 %1 ->
  (Either s1 s2 %1 -> Linear.IO a) %1 -> Linear.IO a
offerEither s match = do  (e, ()) <- recv s
                          match e
\end{spec}

\paragraph{Deadlock freedom}

\todo{%
	The session-typed channels as presented thus far can be used to write deadlocking programs. There's two ways of ruling out deadlocking programs. First, we can limit programs by ensuring that there's always \emph{at most} one way to get a message from one channel to another. We can do this by hiding |new| and only exposing |connect|, which creates a new channel and \emph{immediately} shares it between two threads. This one's simple, but it's inexpressive. The other option is priorities, which we discuss in~\cref{sec:priority-sesh}.}

\begin{spec}
connect ::  Session s => (s %1 -> Linear.IO ()) %1 ->
            (Dual s %1 -> Linear.IO a) %1 -> Linear.IO a
connect k1 k2 = do (s1, s2) <- new; fork (k1 s1); k2 s2
\end{spec}


\subsection{Session-typed channels with priority}\label{sec:priority-sesh}
We conclude by decorating the session-typed channels with \emph{priorities} to ensure deadlock freedom.

\paragraph{Priorities}
Priorities are either |Bot|, a~natural number, or |Top|. A~natural number priority represents abstractly the \emph{time} at which some action happens---the lower the number, the sooner it happens. The values |Top| and |Bot| are used as the identities for |`Min`| and |`Max`| in lower and upper bounds on priorities, respectively. We let |o| range over natural numbers, |p| over \emph{lower bounds}, and |q| over \emph{upper bounds}.

\begin{spec}
data Priority = Bot | Val Nat | Top
\end{spec}

We define strict inequality ($|`LT`|$), minimum (|`Min`|), and maximum (|`Max`|) on priorities as expected.

\paragraph{Channels}
We define |Send o|, |Recv o|, and |End o|, which decorate the \emph{raw} sessions from~\cref{sec:sesh} with the priority |o| of the communication action, \ie, it denoted when the communication happens. Duality (|Dual|) preserves these priorities. Operationally, these types are mere wrappers.

\paragraph{The communication monad}
We define a graded monad |Sesh p q|, which decorates |Linear.IO| with a lower bound |p| and an upper bound |q| on the priorities of its communication actions, \ie, if you run the monad, it denotes when communication begins and ends.

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
\item |new|, |fork|, and |cancel| don't perform any communication action, and so return a \emph{pure} computation of type |Sesh Top Bot|.
\end{itemize*}

\begin{spec}
send     :: Session s => (a, Send o a s) %1 -> Sesh (Val o) (Val o) s
recv     :: Session s => Recv o a s %1 -> Sesh (Val o) (Val o) (a, s)
close    :: End o %1 -> Sesh (Val o) (Val o) ()
new      :: Session s => Sesh Top Bot (s, Dual s)
fork     :: Sesh p q () %1 -> Sesh Top Bot ()
cancel   :: Session s => s %1 -> Sesh Top Bot ()
\end{spec}

\paragraph{Safe IO}
\todo{%
	We can decorate the communication monad and channel types with tokens \`a la |runST|, which allows to safely implement |runSesh :: (forall tok. Sesh p q tok a) %1 -> a| using |unsafePerformIO|.}

\paragraph{Recursion}
\todo{%
	We can write priority-polymorphic types and use those to implement recursive sessions, \`a la~\citet{padovaninovara15}.}
