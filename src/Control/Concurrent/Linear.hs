module Control.Concurrent.Linear
  ( ThreadId
  , forkIO
  , forkIO_
  ) where

import Prelude.Linear
import Control.Concurrent       (ThreadId)
import Control.Concurrent       qualified as Unrestricted (forkIO)
import Data.Functor.Linear      qualified as Linear (void)
import Data.Unrestricted.Linear (Movable(..))
import System.IO.Linear         qualified as Linear
import Unsafe.Linear            qualified as Unsafe (coerce, toLinear)

-- |Creates a new thread to run the 'IO' computation passed as the first
--  argument, and returns the 'ThreadId' of the newly created thread.
forkIO :: Linear.IO () %1 -> Linear.IO ThreadId
forkIO action = Linear.fromSystemIO (forkSystemIO (toSystemIO action))
  where
    forkSystemIO :: IO () %1 -> IO ThreadId
    forkSystemIO = Unsafe.toLinear Unrestricted.forkIO
    toSystemIO :: Linear.IO a %1 -> IO a
    toSystemIO = Unsafe.coerce

-- |Variant of 'forkIO' which discards the 'ThreadId'.
forkIO_ :: Linear.IO () %1 -> Linear.IO ()
forkIO_ action = Linear.void (forkIO action)

instance Consumable ThreadId where
  consume threadId = Unsafe.toLinear (const ()) threadId
