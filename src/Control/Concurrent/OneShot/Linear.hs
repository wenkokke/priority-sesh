{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Control.Concurrent.OneShot.Linear where

import Prelude qualified as Unrestricted
import Prelude.Linear
import Control.Functor.Linear qualified as Linear
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
import GHC.Stack.CCS (whoCreated)


data CommunicationException
  = OtherEndpointCancelledException
  deriving Show

instance Exception CommunicationException


-- * One-shot channels

newtype SendOnce a = SendOnce (Weak (MVar a))
newtype RecvOnce a = RecvOnce (Weak (MVar a))

instance Consumable (SendOnce a) where
  -- This is safe because 'SendOnce' and 'RecvOnce' are only ever
  -- created via 'new', which adds a finaliser which invokes consume
  -- on any potential value contained in the MVar.
  consume x = Unsafe.toLinear2 const () x

instance Consumable (RecvOnce a) where
  -- This is safe because 'SendOnce' and 'RecvOnce' are only ever
  -- created via 'new', which adds a finaliser which invokes consume
  -- on any potential value contained in the MVar.
  consume x = Unsafe.toLinear2 const () x

new :: Consumable a => Linear.IO (SendOnce a, RecvOnce a)
new = Linear.fromSystemIO newSystemIO
  where
    newSystemIO :: Consumable a => IO (SendOnce a, RecvOnce a)
    newSystemIO =
      -- Use the unrestricted monadic bind.
      let (>>=) = (Unrestricted.>>=) in do
        mvar <- Unrestricted.newEmptyMVar
        let finalizer :: IO ()
            finalizer = Unrestricted.tryTakeMVar mvar >>= \case
              Nothing  -> Unrestricted.return ()
              Just val -> Unrestricted.return (consume val)
        weak <- Unrestricted.mkWeakMVar mvar finalizer
        Unrestricted.return (SendOnce weak, RecvOnce weak)

send :: Consumable a => SendOnce a %1 -> a %1 -> Linear.IO ()
send (SendOnce weak) x = Linear.deRefWeak weak Linear.>>= \case
  Nothing   -> Linear.return (consume x)
  Just mvar -> Linear.putMVar mvar x

recv :: RecvOnce a %1 -> Linear.IO a
recv (RecvOnce weak) = Linear.deRefWeak weak Linear.>>= \case
  Nothing   -> Linear.throwIO OtherEndpointCancelledException
  Just mvar -> Linear.takeMVar mvar


-- * Synchronisation construct

newtype SyncOnce = SyncOnce (SendOnce (), RecvOnce ())
  deriving Consumable

newSync :: Linear.IO (SyncOnce, SyncOnce)
newSync =
  -- Use the linear monadic bind.
  let (>>=) = (Linear.>>=) in do
    (ch_s1, ch_r1) <- new
    (ch_s2, ch_r2) <- new
    Linear.return (SyncOnce (ch_s1, ch_r2), SyncOnce (ch_s2, ch_r1))

sync :: SyncOnce %1 -> Linear.IO ()
sync (SyncOnce (ch_s, ch_r)) =
  -- Use the linear monadic bind.
  let (>>) = (Linear.>>) in do
    send ch_s ()
    recv ch_r

-- -}
-- -}
-- -}
-- -}
-- -}
