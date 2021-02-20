{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.HUnit.Linear where

import           Test.HUnit
import           Control.Exception (catch, BlockedIndefinitelyOnMVar(..))
import qualified Control.Monad.Linear as Linear
import qualified Prelude
import           Prelude.Linear hiding (Dual)
import qualified System.IO.Linear as Linear

instance Assertable t => Assertable (Linear.IO (Ur t)) where
  assert x = assert (Linear.withLinearIO x)

assertBlockedIndefinitelyOnMVar :: Linear.IO (Ur ()) -> IO ()
assertBlockedIndefinitelyOnMVar act = assert (catch body handler)
  where
    body = Linear.withLinearIO (act Linear.>>= \(Ur ()) -> Linear.return (Ur False))
    handler BlockedIndefinitelyOnMVar = Prelude.return True
