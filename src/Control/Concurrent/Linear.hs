{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Concurrent.Linear
  ( ThreadId
  , forkIO
  , forkLinearIO
  ) where

import           Prelude.Linear
import           Control.Concurrent (ThreadId)
import qualified Control.Concurrent as NonLinear
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe

forkIO :: IO () %1-> IO ThreadId
forkIO p = Unsafe.toLinear (NonLinear.forkIO) p

forkLinearIO :: Linear.IO () %1-> Linear.IO ThreadId
forkLinearIO p = Linear.fromSystemIO (forkIO (Unsafe.coerce p))

instance Consumable ThreadId where
  consume threadId = Unsafe.toLinear (\_ -> ()) threadId
