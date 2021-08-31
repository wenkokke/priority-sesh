{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Concurrent.Channel.Session.DF
  ( Sesh,
    runSeshIO,
    runSesh,
    ireturn,
    (>>>=),
    (>>>),
    SessionToken,
    Send,
    Recv,
    End,
    Session (..),
    OneShot.CommunicationException (..),
    send,
    recv,
    close,
    fork,
    offer,
    select,
  )
where

import Control.Concurrent.Channel.OneShot qualified as OneShot
import Control.Concurrent.Linear (forkIO_)
import Control.Functor.Linear (Functor (..), Monad (..), return)
import Data.Kind (Type)
import Data.Type.Period (At, Empty, Period (..), type (+), type (<))
import Data.Type.Priority (Priority (..))
import Data.Unrestricted.Linear (Consumable (..), Ur (..), unur)
import GHC.TypeNats (Nat)
import GHC.TypeNats qualified as Nat
import Prelude.Linear (($))
import System.IO qualified as System (IO)
import System.IO.Linear qualified as Linear
import System.IO.Linear.Cancelable (Cancelable)
import System.IO.Unsafe (unsafePerformIO)
import Unsafe.Linear qualified as Unsafe

-- | Session tokens are used to ensure that session-typed channels cannot escape
--   the 'Sesh' monad, following the 'T' monad.
--
--   To instantiate a session token, use 'runSesh' or 'runSeshIO'.
data SessionToken = SessionToken

-- | The monad for deadlock-free session-typed communication.
--   The monad has four parameters:
--
--   [@t@]  The session-token, which ensures channels do not escape the session
--           monad.
--
--   [@pb@]  The lower and upper bounds on the priority, i.e., if we evaluate
--           action, at what time step does it begin and end communicating? If
--           the action does not communicate, this should be 'Empty'.
--
--   [@a@]   The type of the value returned by the action.
newtype Sesh (t :: SessionToken) (p :: Period) (a :: Type)
  = Sesh (Linear.IO a)

-- | Unpack the 'Sesh' monad.
--
-- NOTE: This operation is /unsafe/ and should not be exported.
unsafeRunSeshIO :: Sesh t p a %1 -> Linear.IO a
unsafeRunSeshIO (Sesh action) = action

-- | Runs a 'Sesh' action in 'Linear.IO'.
runSeshIO :: (forall t. Sesh t p a) %1 -> Linear.IO a
runSeshIO action = unsafeRunSeshIO (action @'SessionToken)

-- | Runs a 'Sesh' action, encapsulating the usage of 'IO'.
runSesh :: (forall t. Sesh t p (Ur a)) %1 -> a
runSesh action = runIO (toSystemIO (runSeshIO action))
  where
    runIO :: System.IO a %1 -> a
    runIO action = Unsafe.toLinear unsafePerformIO action
    toSystemIO :: Linear.IO (Ur a) %1 -> System.IO a
    toSystemIO action = Unsafe.coerce (fmap unur action)

-- | Inject a value into the 'Sesh' type. See 'return'.
ireturn :: a %1 -> Sesh t Empty a
ireturn a = Sesh $ return a

-- | Sequentially compose two 'Sesh' actions, passing any value produced by the
--   first as an argument to the second. See '>>='.
(>>>=) :: (p1 < p2) => Sesh t p1 a %1 -> (a %1 -> Sesh t p2 b) %1 -> Sesh t (p1 + p2) b
action >>>= f = Sesh $ unsafeRunSeshIO action >>= \a -> unsafeRunSeshIO (f a)

-- | Sequentially compose two 'Sesh' actions, discarding the unit value produced
--   by the first. See '>>'.
(>>>) :: (p1 < p2) => Sesh t p1 () %1 -> Sesh t p2 b %1 -> Sesh t (p1 + p2) b
action1 >>> action2 = action1 >>>= \() -> action2

-- | A channel endpoint over which one sends an 'a' before continuing as 's'.
newtype Send (t :: SessionToken) (o :: Nat) (a :: Type) (s :: Type) where
  Send :: OneShot.SendOnce (a, Dual s) %1 -> Send t o a s

-- | A channel endpoint over which one receives an 'a' before continuing as 's'.
newtype Recv (t :: SessionToken) (o :: Nat) (a :: Type) (s :: Type) where
  Recv :: OneShot.RecvOnce (a, s) %1 -> Recv t o a s

-- | A channel endpoint over which one synchronised before ending the session.
newtype End (t :: SessionToken) (o :: Nat) where
  End :: OneShot.SyncOnce %1 -> End t o

-- | The class of session types.
class (Consumable s, Session (Dual s), Dual (Dual s) ~ s) => Session s where
  -- | Duality for session types, which ensures that two communicating processes
  -- act in dual ways, e.g., if one process sends the other receives.
  type Dual s = result | result -> s

  -- | Creates a new session-typed channel with two dual endpoints.
  new :: Sesh t Empty (s, Dual s)

deriving instance Consumable s => Consumable (Send t o a s)

deriving instance Consumable s => Consumable (Recv t o a s)

deriving instance Consumable (End t o)

deriving instance Cancelable s => Cancelable (Send t o a s)

deriving instance Cancelable s => Cancelable (Recv t o a s)

deriving instance Cancelable (End t o)

instance (Consumable a, Session s) => Session (Send t o a s) where
  type Dual (Send t o a s) = Recv t o a (Dual s)
  new = Sesh $ do
    (sendOnce, recvOnce) <- OneShot.new
    return (Send sendOnce, Recv recvOnce)

instance (Consumable a, Session s) => Session (Recv t o a s) where
  type Dual (Recv t o a s) = Send t o a (Dual s)
  new = Sesh $ do
    (sendOnce, recvOnce) <- OneShot.new
    return (Recv recvOnce, Send sendOnce)

instance Session (End t o) where
  type Dual (End t o) = End t o
  new = Sesh $ do
    (syncOnce1, syncOnce2) <- OneShot.newSync
    return (End syncOnce1, End syncOnce2)

instance Session () where
  type Dual () = ()
  new = ireturn ((), ())

-- | Send a value over a channel.
--   Returns the channel for the rest of the session.
send :: forall o t a s. (Consumable a, Session s) => (a, Send t o a s) %1 -> Sesh t (At o) s
send (a, Send sendOnce) = Sesh $ do
  (here, there) <- unsafeRunSeshIO new
  OneShot.send sendOnce (a, there)
  return here

-- | Receive a value over a channel.
--   Returns the value paired with the channel for the rest of the session.
recv :: forall o t a s. Recv t o a s %1 -> Sesh t (At o) (a, s)
recv (Recv recvOnce) =
  Sesh $
    OneShot.recv recvOnce

-- | Close a finished channel.
close :: forall o t. End t o %1 -> Sesh t (At o) ()
close (End syncOnce) =
  Sesh $
    OneShot.sync syncOnce

-- | Create a child process.
fork :: Sesh t (begin :-: end) () %1 -> Sesh t (begin :-: 'Bot) ()
fork action =
  Sesh $
    forkIO_ (unsafeRunSeshIO action)

-- |
offer :: forall o p s t op a. (At o < p) => Recv t o op () %1 -> (op %1 -> Sesh t p a) %1 -> Sesh t (At o + p) a
offer s match =
  recv s >>>= \(op, ()) -> match op

-- |
select :: forall o s t op. (Session s, Consumable op) => (Dual s %1 -> op) -> Send t o op () -> Sesh t (At o) s
select label s =
  new >>>= \(here, there) ->
    send (label there, s)
      >>> ireturn here

-- |
type family Lift (o :: Nat) (s :: Type) :: Type where
  Lift o1 (Send t o2 a s) = Send t (o1 Nat.+ o2) a (Lift o1 s)
  Lift o1 (Recv t o2 a s) = Recv t (o1 Nat.+ o2) a (Lift o1 s)
  Lift o1 (End t o2) = End t (o1 Nat.+ o2)
