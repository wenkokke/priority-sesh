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
    connect,
    cancel,
    Offer,
    Select,
    offer,
    select,
  )
where

import Control.Concurrent.Channel.OneShot qualified as OneShot
import Control.Concurrent.Linear (forkIO_)
import Control.Functor.Linear (Functor (..), Monad (..), return)
import Data.Kind (Type)
import Data.Type.Period (At, Empty, ParallelTo, Period (..), type (<), type (<>))
import Data.Type.Priority (Priority (..))
import Data.Unrestricted.Linear (Consumable (..), Movable (..), Ur (..))
import GHC.TypeNats (Nat, type (+))
import Prelude.Linear (($))
import System.IO qualified as System (IO)
import System.IO.Linear qualified as Linear
import System.IO.Linear.Cancelable (Cancelable)
import System.IO.Linear.Cancelable qualified as Cancelable
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
runSesh :: Movable a => (forall t. Sesh t p a) %1 -> a
runSesh action = runIO (toSystemIO (runSeshIO action))
  where
    runIO :: System.IO a %1 -> a
    runIO action = Unsafe.toLinear unsafePerformIO action
    toSystemIO :: Movable a => Linear.IO a %1 -> System.IO a
    toSystemIO action = Unsafe.coerce action

-- | Inject a value into the 'Sesh' type. See 'return'.
ireturn :: a %1 -> Sesh t Empty a
ireturn a = Sesh $ return a

-- | Sequentially compose two 'Sesh' actions, passing any value produced by the
--   first as an argument to the second. See '>>='.
(>>>=) :: (p1 < p2) => Sesh t p1 a %1 -> (a %1 -> Sesh t p2 b) %1 -> Sesh t (p1 <> p2) b
action >>>= f = Sesh $ unsafeRunSeshIO action >>= \a -> unsafeRunSeshIO (f a)

-- | Sequentially compose two 'Sesh' actions, discarding the unit value produced
--   by the first. See '>>'.
(>>>) :: (p1 < p2) => Sesh t p1 () %1 -> Sesh t p2 b %1 -> Sesh t (p1 <> p2) b
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
fork :: Sesh t p () %1 -> Sesh t (ParallelTo p) ()
fork action =
  Sesh $
    forkIO_ (unsafeRunSeshIO action)

-- | Create a new thread and connect it to the current thread with a new channel.
connect :: (ParallelTo p < p, Session s) => (s %1 -> Sesh t p ()) %1 -> Sesh t (ParallelTo p) (Dual s)
connect child = new >>>= \(here, there) -> fork (child there) >>> ireturn here

-- | Variant of 'Cancelable.cancel' which runs in the 'Sesh' monad.
cancel :: Cancelable s => s %1 -> Sesh t Empty ()
cancel s = Sesh $ Cancelable.cancel s

-- | An alias for receiving a single choice operator, e.g., 'Either s1 s2'.
type Offer (t :: SessionToken) (o :: Nat) (op :: SessionToken -> Nat -> Type) = Recv t o (op t (o + 1)) ()

-- | Helper for offering choice based on a data type.
--
-- @
--     data CalcOp t o
--       = Neg (Recv t o Int (Send t (o + 2) Int (End t (o + 3))))
--       | Add (Recv t o Int (Recv t (o + 1) Int (Send t (o + 2) Int (End t (o + 3)))))
--
--     calcServer :: Recv t 0 (CalcOp t 1) () %1 -> Sesh t ('Val 0 :-: 'Val 4) ()
--     calcServer s = offer s $ \case
--       -- Offer negation:
--       Neg s -> do
--         (x, s) <- recv s
--         s <- send (negate x, s)
--         close s
--
--       -- Offer addition:
--       Add s -> do
--         (x, s) <- recv s
--         (y, s) <- recv s
--         s <- send (x + y, s)
--         close s
-- @
offer :: forall o p s t op a. (At o < p) => Offer t o op %1 -> (op t (o + 1) %1 -> Sesh t p a) %1 -> Sesh t (At o <> p) a
offer s match = recv s >>>= \(op, ()) -> match op

-- | An alias for sending a single choice operator, e.g., 'Either s1 s2'
type Select (t :: SessionToken) (o :: Nat) (op :: SessionToken -> Nat -> Type) = Send t o (op t (o + 1)) ()

-- | Helper for selecting choice based on a data type.
--
-- @
--     data CalcOp t o
--       = Neg (Recv t o Int (Send t (o + 2) Int (End t (o + 3))))
--       | Add (Recv t o Int (Recv t (o + 1) Int (Send t (o + 2) Int (End t (o + 3)))))
--
--     negClient :: Send t 0 (CalcOp t 1) () %1 -> Sesh t ('Val 0 :-: 'Val 4) Bool
--     negClient s0 = do
--       s1 <- select Neg s0
--       s2 <- send (42, s1)
--       (r, s3) <- recv s2
--       close s3
--       return (r == -42)
-- @
select :: forall o s t op. (Session s, Consumable (op t (o + 1))) => (Dual s %1 -> op t (o + 1)) %1 -> Select t o op %1 -> Sesh t (At o) s
select label s = new >>>= \(here, there) -> send (label there, s) >>> ireturn here

-- |
type family Lift (o :: Nat) (s :: Type) :: Type where
  Lift o1 (Send t o2 a s) = Send t (o1 + o2) a (Lift o1 s)
  Lift o1 (Recv t o2 a s) = Recv t (o1 + o2) a (Lift o1 s)
  Lift o1 (End t o2) = End t (o1 + o2)
