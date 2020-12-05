{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RebindableSyntax #-}

module Test.HUnit.Linear where

import           Test.HUnit
import           Prelude.Linear hiding (Dual)
import qualified System.IO.Linear as Linear

instance Assertable t => Assertable (Linear.IO (Ur t)) where
  assert x = assert (Linear.withLinearIO x)
