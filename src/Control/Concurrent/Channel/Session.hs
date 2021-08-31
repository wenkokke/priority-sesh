{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Concurrent.Channel.Session
  ( Send,
    Recv,
    End,
    Session (..),
    OneShot.CommunicationException (..),
    send,
    recv,
    close,
    connect,
    select,
    offer,
  )
where

import Control.Concurrent.Channel.OneShot qualified as OneShot
import Control.Concurrent.Linear (forkIO_)
import Control.Functor.Linear (Monad (..), return)
import Data.Unrestricted.Linear (Consumable (..), lseq)
import System.IO.Linear qualified as Linear (IO)
import System.IO.Linear.Cancelable (Cancelable (..))

-- | A channel endpoint over which one sends an 'a' before continuing as 's'.
newtype Send a s where
  Send :: OneShot.SendOnce (a, Dual s) %1 -> Send a s

-- | A channel endpoint over which one receives an 'a' before continuing as 's'.
newtype Recv a s where
  Recv :: OneShot.RecvOnce (a, s) %1 -> Recv a s

-- | A channel endpoint over which one synchronised before ending the session.
newtype End where
  End :: OneShot.SyncOnce %1 -> End

-- | The class of session types.
class (Consumable s, Session (Dual s), Dual (Dual s) ~ s) => Session s where
  -- | Duality for session types, which ensures that two communicating processes
  --   act in dual ways, e.g., if one process sends the other receives.
  type Dual s = result | result -> s

  -- | Creates a new session-typed channel with two dual endpoints.
  --
  --   NOTE: 'new' can be used to construct deadlocking processes, e.g.,
  --   @
  --       woops :: IO ()
  --       woops = do
  --         (sender1, receiver1) <- new
  --         (sender2, receiver2) <- new
  --         forkIO_ $ do
  --           (void, ()) <- recv receiver1
  --           send (sender2, void)
  --         (void, ()) <- recv receiver2
  --         send (sender1, void)
  --   @
  --
  --   If you want to /ensure/ that no deadlocks occur, you can use 'connect',
  --   or the session-typed channels with priorities from
  --   'Control.Concurrent.Channel.Session.DF'.
  new :: Linear.IO (s, Dual s)

deriving instance Consumable s => Consumable (Send a s)

deriving instance Consumable s => Consumable (Recv a s)

deriving instance Consumable End

deriving instance Cancelable s => Cancelable (Send a s)

deriving instance Cancelable s => Cancelable (Recv a s)

deriving instance Cancelable End

instance (Consumable a, Session s) => Session (Send a s) where
  type Dual (Send a s) = Recv a (Dual s)
  new = do
    (sendOnce, recvOnce) <- OneShot.new
    return (Send sendOnce, Recv recvOnce)

instance (Consumable a, Session s) => Session (Recv a s) where
  type Dual (Recv a s) = Send a (Dual s)
  new = do
    (sendOnce, recvOnce) <- OneShot.new
    return (Recv recvOnce, Send sendOnce)

instance Session End where
  type Dual End = End
  new = do
    (syncOnce1, syncOnce2) <- OneShot.newSync
    return (End syncOnce1, End syncOnce2)

instance Session () where
  type Dual () = ()
  new = return ((), ())

-- | Send a value over a channel.
--   Returns the channel for the rest of the session.
send :: (Consumable a, Session s) => (a, Send a s) %1 -> Linear.IO s
send (a, Send sendOnce) = do
  (here, there) <- new
  OneShot.send sendOnce (a, there)
  return here

-- | Receive a value over a channel.
--   Returns the value paired with the channel for the rest of the session.
recv :: Recv a s %1 -> Linear.IO (a, s)
recv (Recv recvOnce) = OneShot.recv recvOnce

-- | Close a finished channel.
close :: End %1 -> Linear.IO ()
close (End syncOnce) = OneShot.sync syncOnce

-- | Create a new thread and connect it to the current thread with a new channel.
connect :: Session s => (s %1 -> Linear.IO ()) %1 -> Linear.IO (Dual s)
connect child = do
  (here, there) <- new
  forkIO_ (child there)
  return here

-- | An alias for receiving a single choice operator, e.g., 'Either s1 s2'.
type Offer op = Recv op ()

-- | Helper for offering choice based on a data type.
--
-- @
--     data CalcOp
--       = Neg (Recv Int (Send Int End))
--       | Add (Recv Int (Recv Int (Send Int End)))
--
--     calcServer :: Offer CalcOp %1 -> Linear.IO ()
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
offer :: Offer op %1 -> (op %1 -> Linear.IO a) %1 -> Linear.IO a
offer s match = do
  (op, unit) <- recv s
  unit `lseq` match op

-- | An alias for sending a single choice operator, e.g., 'Either s1 s2'.
type Select op = Send op ()

-- | Helper for selecting choice based on a data type.
--
-- @
--     data CalcOp
--       = Neg (Recv Int (Send Int End))
--       | Add (Recv Int (Recv Int (Send Int End)))
--
--     negClient :: Select CalcOp %1 -> Linear.IO Bool
--     negClient s = do
--       s <- select Neg s
--       s <- send (42, s)
--       (r, s) <- recv s
--       close s
--       return (r == -42)
-- @
select :: (Session s, Consumable op) => (Dual s %1 -> op) %1 -> Send op () %1 -> Linear.IO s
select label s = do
  (here, there) <- new
  send (label there, s)
  return here
