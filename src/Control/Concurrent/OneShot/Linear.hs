{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Concurrent.OneShot.Linear
  -- One-shot channels
  ( SendOnce
  , RecvOnce
  , new
  , send
  , recv
  -- One-shot synchronisation
  , SyncOnce
  , newSync
  , sync
  ) where

import           Prelude.Linear
import           Control.Concurrent.Linear
import           Control.Concurrent.MVar.Linear
import           Control.Functor.Linear
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

instance Consumable (SendOnce a) where
  consume (SendOnce mvar) = consume mvar

instance Consumable (RecvOnce a) where
  consume (RecvOnce mvar) = consume mvar


-- * Synchronisation construct

data SyncOnce = SyncOnce (SendOnce ()) (RecvOnce ())

newSync :: Linear.IO (SyncOnce, SyncOnce)
newSync = do
  (ch_s1, ch_r1) <- new
  (ch_s2, ch_r2) <- new
  return (SyncOnce ch_s1 ch_r2, SyncOnce ch_s2 ch_r1)

sync :: SyncOnce %1 -> Linear.IO ()
sync (SyncOnce ch_s ch_r) = do send ch_s (); recv ch_r

instance Consumable SyncOnce where
  consume (SyncOnce ch_s ch_r) = consume (ch_s, ch_r)
