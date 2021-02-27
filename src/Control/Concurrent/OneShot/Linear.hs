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

instance Consumable (SendOnce a) where
  consume (SendOnce mvar) = consume mvar

instance Consumable (RecvOnce a) where
  consume (RecvOnce mvar) = consume mvar


-- * Synchronisation construct

newtype SyncOnce = SyncOnce (SendOnce (), RecvOnce ())

newSync :: Linear.IO (SyncOnce, SyncOnce)
newSync = do
  (mvar_s1, mvar_r1) <- new
  (mvar_s2, mvar_r2) <- new
  return (SyncOnce (mvar_s1, mvar_r2), SyncOnce (mvar_s2, mvar_r1))

sync :: SyncOnce %1 -> Linear.IO ()
sync (SyncOnce (mvar_s, mvar_r)) = do
  send mvar_s ()
  recv mvar_r

instance Consumable SyncOnce where
  consume (SyncOnce (mvar_s, mvar_r)) = consume (mvar_s, mvar_r)
