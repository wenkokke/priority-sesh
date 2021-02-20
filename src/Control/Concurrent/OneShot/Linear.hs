{-# LANGUAGE GADTs #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}

module Control.Concurrent.OneShot.Linear
  ( Sender
  , Receiver
  , new
  , send
  , receive
  ) where

import           Prelude.Linear
import           Control.Concurrent.MVar.Linear
import           Control.Monad.Linear
import           Data.Bifunctor.Linear (bimap)
import           Data.Proxy
import           Data.Unrestricted.Linear
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe


data Sender a where
  Sender :: MVar a %1 -> Sender a

data Receiver a where
  Receiver :: MVar a %1 -> Receiver a


new :: Linear.IO (Sender a, Receiver a)
new = bimap (Sender . unur) (Receiver . unur) . dup2 <$> newEmptyMVar

send :: Sender a %1 -> a %1 -> Linear.IO (Ur ())
send (Sender mvar) x = do
  putMVar mvar x
  return $ Ur ()

receive :: Receiver a %1 -> Linear.IO a
receive (Receiver mvar) =
  takeMVar mvar
