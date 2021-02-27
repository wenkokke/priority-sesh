{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE FlexibleInstances #-}

module Test.HUnit.Linear where

import           Test.HUnit
import           Control.Exception (catch, BlockedIndefinitelyOnMVar(..))
import           Control.Functor.Linear
import qualified Prelude
import           Prelude.Linear hiding (Dual)
import qualified System.IO.Linear as Linear

instance Assertable t => Assertable (Ur t) where
  assert (Ur x) = assert x

instance (Assertable t, Movable t) => Assertable (Linear.IO t) where
  assert x = assert (Linear.withLinearIO (fmap move x))

assertBlockedIndefinitelyOnMVar :: Consumable t => Linear.IO t -> IO ()
assertBlockedIndefinitelyOnMVar act = assert (body `catch` handler)
  where
    body = Linear.withLinearIO (act >>= \x -> x `lseq` return (Ur False))
    handler BlockedIndefinitelyOnMVar = Prelude.return True
