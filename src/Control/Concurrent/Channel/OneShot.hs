{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE StandaloneDeriving #-}

module Control.Concurrent.Channel.OneShot
  ( -- * One-shot channels
    SendOnce,
    RecvOnce,
    new,
    send,
    recv,
    CommunicationException (..),

    -- * Synchronisation
    SyncOnce,
    newSync,
    sync,
  )
where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Control.Exception (BlockedIndefinitelyOnMVar (..), Exception, catch, throwIO)
import Control.Functor.Linear qualified as Linear (Monad (..), return)
import Control.Monad qualified as Unrestricted (Monad (..))
import Data.Unrestricted.Linear (Consumable (..), Ur (..))
import Prelude.Linear
import System.IO qualified as Unrestricted (IO)
import System.IO.Linear qualified as Linear (IO, fromSystemIO, fromSystemIOU)
import System.IO.Linear.Cancelable (Cancelable (..))
import System.Mem.Weak (Weak)
import System.Mem.Weak qualified as Weak
import Unsafe.Linear qualified as Unsafe (toLinear)

data CommunicationException
  = SenderCanceled
  deriving (Show)

instance Exception CommunicationException

-- | A one-shot sender channel.
data SendOnce a where
  SendOnce :: Weak (MVar a) -> SendOnce a

-- | A one-shot receiver channel.
data RecvOnce a where
  RecvOnce :: Weak (MVar a) -> RecvOnce a

-- A note on 'Consumable' and 'Cancelable' for one-shot channels.
--
-- The 'Consumable' instance is /mostly/ safe.
--
-- The implementation drops the reference to the weak pointer. When the last
-- weak reference is dropped and the garbage collector collects the 'MVar', it
-- calls 'finalizeMVar', which calls 'consume' on the contents of the 'MVar'.
--
-- If it's possible, it's preferred to call 'cancel' (see below) on the channels
-- instead, which does not rely on the garbage collector.
--

instance Consumable (SendOnce a) where
  consume (SendOnce _weak) = ()

instance Consumable (RecvOnce a) where
  consume (RecvOnce _weak) = ()

instance Cancelable (SendOnce a) where
  cancel (SendOnce weak) = cancel weak

instance Cancelable (RecvOnce a) where
  cancel (RecvOnce weak) = cancel weak

-- | Create a new one-shot channel.
new :: Consumable a => Linear.IO (SendOnce a, RecvOnce a)
new =
  Linear.fromSystemIO $
    let (>>=) = (Unrestricted.>>=)
     in do
          mvar <- MVar.newEmptyMVar
          weak <- MVar.mkWeakMVar mvar (finalizeMVar mvar)
          Unrestricted.return (SendOnce weak, RecvOnce weak)
  where
    finalizeMVar :: Consumable a => MVar a -> IO ()
    finalizeMVar mvar =
      let (>>=) = (Unrestricted.>>=)
       in do
            maybeValue <- MVar.tryTakeMVar mvar
            case maybeValue of
              Nothing -> Unrestricted.return ()
              Just value -> Unrestricted.return (consume value)

-- | Send a value over a one-shot sender.
send :: Consumable a => SendOnce a %1 -> a %1 -> Linear.IO ()
send sender a =
  let (>>=) = (Linear.>>=); fail = error
   in do
        Ur maybeMVar <- unsafeDeRefSendOnce sender
        case maybeMVar of
          Nothing -> Linear.return (consume a)
          Just mvar -> unsafePutMVar mvar a
  where
    -- Dereference a one-shot sender to an unrestricted 'MVar'.
    unsafeDeRefSendOnce :: SendOnce a %1 -> Linear.IO (Ur (Maybe (MVar a)))
    unsafeDeRefSendOnce (SendOnce weak) = Linear.fromSystemIOU (Weak.deRefWeak weak)

    -- Variant of 'MVar.putMVar' in 'Linear.IO' which is linear in the value put.
    unsafePutMVar :: MVar a -> a %1 -> Linear.IO ()
    unsafePutMVar mvar a = Linear.fromSystemIO (Unsafe.toLinear (MVar.putMVar mvar) a)

-- | Receive a value over a one-shot receiver.
recv :: RecvOnce a %1 -> Linear.IO a
recv (RecvOnce weak) =
  Linear.fromSystemIO $
    let (>>=) = (Unrestricted.>>=)
     in do
          maybeMVar <- Weak.deRefWeak weak
          case maybeMVar of
            Nothing -> throwIO SenderCanceled
            Just mvar -> do
              -- NOTE: 'takeMVar' may throw a 'BlockedIndefinitelyOnMVar' exception
              MVar.takeMVar mvar `catch` \BlockedIndefinitelyOnMVar -> throwIO SenderCanceled

-- * Synchronisation

-- | A naive one-shot synchronisation, implemented using two one-shot channels.
newtype SyncOnce where
  SyncOnce :: (SendOnce (), RecvOnce ()) %1 -> SyncOnce

deriving instance Consumable SyncOnce

deriving instance Cancelable SyncOnce

-- | Create a one-shot synchronisation.
newSync :: Linear.IO (SyncOnce, SyncOnce)
newSync =
  let (>>=) = (Linear.>>=)
   in do
        (sender1, receiver1) <- new
        (sender2, receiver2) <- new
        Linear.return
          ( SyncOnce (sender1, receiver2),
            SyncOnce (sender2, receiver1)
          )

-- | Synchronise on a one-shot synchronisation.
sync :: SyncOnce %1 -> Linear.IO ()
sync (SyncOnce (sender, receiver)) =
  let (>>) = (Linear.>>)
   in do
        send sender ()
        recv receiver
