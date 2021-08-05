module Control.Concurrent.MVar.Linear
  ( MVar
  , newEmptyMVar
  , takeMVar
  , tryTakeMVar
  , putMVar
  , mkWeakMVar
  ) where

import Prelude (IO)
import Prelude.Linear
import Control.Functor.Linear qualified as Control
import Control.Concurrent.MVar (MVar)
import Control.Concurrent.MVar qualified as MVar
import Data.Unrestricted.Linear
import System.IO.Linear qualified as Linear
import System.Mem.Weak.Linear (Weak)
import Unsafe.Linear as Unsafe
import System.IO.Linear (fromSystemIOU)

newEmptyMVar :: Linear.IO (Ur (MVar a))
newEmptyMVar = Linear.fromSystemIOU MVar.newEmptyMVar

takeMVar :: MVar a %1 -> Linear.IO a
takeMVar mvar = Linear.fromSystemIO (Unsafe.toLinear MVar.takeMVar mvar)

tryTakeMVar :: MVar a %1 -> Linear.IO (Maybe a)
tryTakeMVar mvar = Linear.fromSystemIO (Unsafe.toLinear MVar.tryTakeMVar mvar)

putMVar :: MVar a %1 -> a %1 -> Linear.IO ()
putMVar mvar x = Linear.fromSystemIO (Unsafe.toLinear2 MVar.putMVar mvar x)

mkWeakMVar :: IO () -> Ur (MVar a) %1 -> Linear.IO (Ur (Weak (MVar a)))
mkWeakMVar finalizer (Ur mvar) = Linear.fromSystemIOU (MVar.mkWeakMVar mvar finalizer)
