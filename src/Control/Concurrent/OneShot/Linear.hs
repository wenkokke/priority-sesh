{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Concurrent.OneShot.Linear
  ( SendOnce
  , RecvOnce
  , SyncOnce
  , new
  , newSync
  , send
  , recv
  , sync
  ) where

import           Prelude.Linear
import           Control.Concurrent.MVar.Linear
import           Control.Monad.Linear
import           Data.Bifunctor.Linear (bimap)
import           Data.Proxy
import           Data.Unrestricted.Linear
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe



-- * One-shot channels

newtype SendOnce a = SendOnce (MVar a)
newtype RecvOnce a = RecvOnce (MVar a)

new :: Linear.IO (SendOnce a, RecvOnce a)
new = bimap (SendOnce . unur) (RecvOnce . unur) . dup2 <$> newEmptyMVar

send :: SendOnce a %1 -> a %1 -> Linear.IO ()
send (SendOnce mvar) x = putMVar mvar x

recv :: RecvOnce a %1 -> Linear.IO a
recv (RecvOnce mvar) = takeMVar mvar


-- * Synchronisation construct

newtype SyncOnce   = SyncOnce (SendOnce (), RecvOnce ())

newSync :: Linear.IO (SyncOnce, SyncOnce)
newSync = do
  (chan_s1, chan_r1) <- new
  (chan_s2, chan_r2) <- new
  return (SyncOnce (chan_s1, chan_r2), SyncOnce (chan_s2, chan_r1))

sync :: SyncOnce %1 -> Linear.IO ()
sync (SyncOnce (chan_s, chan_r)) = do
  send chan_s ()
  recv chan_r
