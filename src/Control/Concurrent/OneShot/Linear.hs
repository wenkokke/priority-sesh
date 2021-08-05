{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}

module Control.Concurrent.OneShot.Linear where

import Prelude qualified as Unrestricted
import Prelude.Linear
import Control.Functor.Linear qualified as Linear
import Control.Cancellable.Linear ( Cancellable(..) )
import Control.Concurrent.Linear ()
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as Unrestricted
import Control.Concurrent.MVar.Linear qualified as Linear
import Data.Unrestricted.Linear
import System.Mem.Weak (Weak)
import System.Mem.Weak.Linear qualified as Linear
import System.IO.Linear qualified as Linear
import Unsafe.Linear qualified as Unsafe
import Control.Exception (Exception)


data CommunicationException
  = OtherEndpointCancelledException
  deriving Show

instance Exception CommunicationException


-- * One-shot channels

newtype SendOnce a = SendOnce (Weak (MVar a))
newtype RecvOnce a = RecvOnce (Weak (MVar a))

instance Cancellable a => Cancellable (SendOnce a) where
  cancel (SendOnce weak) = Linear.finalize weak

instance Cancellable a => Cancellable (RecvOnce a) where
  cancel (RecvOnce weak) = Linear.finalize weak

new :: Cancellable a => Linear.IO (SendOnce a, RecvOnce a)
new = Linear.fromSystemIO newSystemIO
  where
    newSystemIO :: Cancellable a => IO (SendOnce a, RecvOnce a)
    newSystemIO =
      -- Use the unrestricted monadic bind.
      let (>>=) = (Unrestricted.>>=) in do
        mvar <- Unrestricted.newEmptyMVar
        let finalizer :: IO ()
            finalizer = Unrestricted.tryTakeMVar mvar >>= \case
              Nothing  -> Unrestricted.return ()
              Just val -> cancelSystemIO val
        weak <- Unrestricted.mkWeakMVar mvar finalizer
        Unrestricted.return (SendOnce weak, RecvOnce weak)

send :: Cancellable a => SendOnce a %1 -> a %1 -> Linear.IO ()
send (SendOnce weak) x = Linear.deRefWeak weak Linear.>>= \case
  Nothing   -> cancel x
  Just mvar -> Linear.putMVar mvar x

recv :: RecvOnce a %1 -> Linear.IO a
recv (RecvOnce weak) = Linear.deRefWeak weak Linear.>>= \case
  Nothing   -> Linear.throwIO OtherEndpointCancelledException
  Just mvar -> Linear.takeMVar mvar


-- * Synchronisation construct

data SyncOnce = SyncOnce (SendOnce ()) (RecvOnce ())

newSync :: Linear.IO (SyncOnce, SyncOnce)
newSync =
  -- Use the linear monadic bind.
  let (>>=) = (Linear.>>=) in do
    (ch_s1, ch_r1) <- new
    (ch_s2, ch_r2) <- new
    Linear.return (SyncOnce ch_s1 ch_r2, SyncOnce ch_s2 ch_r1)

sync :: SyncOnce %1 -> Linear.IO ()
sync (SyncOnce ch_s ch_r) =
  -- Use the linear monadic bind.
  let (>>) = (Linear.>>) in do
    send ch_s ()
    recv ch_r

instance Cancellable SyncOnce where
  cancel (SyncOnce ch_s ch_r) =
    let (>>) = (Linear.>>) in do
      cancel ch_s
      cancel ch_r

-- -}
-- -}
-- -}
-- -}
-- -}
