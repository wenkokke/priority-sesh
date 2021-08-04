module Control.Concurrent.MVar.Linear
  ( MVar
  , newEmptyMVar
  , takeMVar
  , putMVar
  ) where

import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Data.Unrestricted.Linear
import System.IO.Linear qualified as Linear
import Unsafe.Linear as Unsafe

newEmptyMVar :: Linear.IO (Ur (MVar a))
newEmptyMVar = Linear.fromSystemIOU MVar.newEmptyMVar

takeMVar :: MVar a %1-> Linear.IO a
takeMVar mvar = Linear.fromSystemIO (Unsafe.toLinear MVar.takeMVar mvar)

putMVar :: MVar a %1-> a %1-> Linear.IO ()
putMVar mvar x = Linear.fromSystemIO (Unsafe.toLinear2 MVar.putMVar mvar x)
