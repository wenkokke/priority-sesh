{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Concurrent.Linear
  ( ThreadId
  , forkIO
  ) where

import           Prelude.Linear
import           Control.Concurrent (ThreadId)
import qualified Control.Concurrent as NonLinear
import           Control.Functor.Linear
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe

-- |Variant of |NonLinear.forkIO| which works on |Linear.IO|.
forkIO :: Linear.IO () %1 -> Linear.IO ThreadId
forkIO = Linear.fromSystemIO . linForkIO . toSystemIO
  where
    toSystemIO :: Linear.IO a %1 -> IO a
    toSystemIO = Unsafe.coerce
    linForkIO :: IO () %1 -> IO ThreadId
    linForkIO = Unsafe.toLinear NonLinear.forkIO

instance Consumable ThreadId where
  consume threadId = Unsafe.toLinear (\_ -> ()) threadId
