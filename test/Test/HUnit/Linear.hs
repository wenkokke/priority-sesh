module Test.HUnit.Linear where

import Prelude
import Prelude.Linear (Consumable(..), Movable(..), Ur(..))
import Test.HUnit
import Control.Exception (catch, BlockedIndefinitelyOnMVar(..))
import Data.Functor.Linear qualified as Data
import System.IO.Linear qualified as Linear
import System.IO.Silently

toSystemIO :: Movable t => Linear.IO t -> IO t
toSystemIO x = Linear.withLinearIO (Data.fmap move x)

toSystemIO_ :: Consumable t => Linear.IO t -> IO ()
toSystemIO_ x = toSystemIO (Data.void x)

instance Assertable t => Assertable (Ur t) where
  assert (Ur x) = assert x

instance (Assertable t, Movable t) => Assertable (Linear.IO t) where
  assert x = assert (toSystemIO x)

assertOutput :: Consumable t => String -> String -> Linear.IO t -> Assertion
assertOutput preface exp act = do
  out <- capture_ (toSystemIO_ act)
  assertEqual preface exp out

assertBlockedIndefinitelyOnMVar :: Consumable t => Linear.IO t -> IO ()
assertBlockedIndefinitelyOnMVar act = do
  assert $ (toSystemIO_ act >> return False) `catch` \BlockedIndefinitelyOnMVar -> return True
