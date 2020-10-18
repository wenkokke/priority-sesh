{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Concurrent.MVar.Linear
  ( MVar
  , newEmptyMVar
  , takeMVar
  , putMVar
  ) where

import           Prelude.Linear hiding (IO)
import           Control.Concurrent.MVar (MVar)
import qualified Control.Concurrent.MVar as MVar
import           Data.Unrestricted.Linear (Ur(..))
import qualified Data.Unrestricted.Linear as Ur
import qualified System.IO.Linear as Linear
import           Unsafe.Linear as Unsafe

newEmptyMVar :: Linear.IO (Ur (MVar a))
newEmptyMVar =
  Linear.fromSystemIOU MVar.newEmptyMVar

takeMVar :: MVar a %1-> Linear.IO a
takeMVar mvar =
  Linear.fromSystemIO (Unsafe.toLinear MVar.takeMVar mvar)

putMVar :: MVar a %1-> a %1-> Linear.IO ()
putMVar mvar x =
  Linear.fromSystemIO (Unsafe.toLinear2 MVar.putMVar mvar x)
