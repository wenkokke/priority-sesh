{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Test.HUnit.Linear
  ( assertOutput
  , assertException
  ) where

import Prelude.Linear
import Control.Exception        (Exception, catch)
import Control.Monad            (Monad(..))
import Data.Proxy               (Proxy(..))
import Data.Unrestricted.Linear (Consumable(..), Movable(..), Ur(..), lseq)
import Test.HUnit               (Assertable(..), Assertion, assertEqual)
import Data.Functor.Linear      qualified as Linear
import System.IO.Linear         qualified as Linear
import System.IO.Silently       (capture_)


instance Assertable t => Assertable (Ur t) where
  assert (Ur x) = assert x


instance (Assertable t, Movable t) => Assertable (Linear.IO t) where
  assert x = assert (Linear.withLinearIO (Linear.fmap move x))


assertOutput :: Consumable t => String -> String -> Linear.IO t -> Assertion
assertOutput preface expected action = do
  output <- capture_ (toSystemIO action)
  assertEqual preface expected output


assertException :: (Exception e, Consumable t) => Proxy e -> Linear.IO t -> Assertion
assertException (Proxy :: Proxy e) action =
  assert ((toSystemIO action >> return False) `catch` (\(_ :: e) -> return True))


toSystemIO :: Consumable t => Linear.IO t -> IO ()
toSystemIO action = Linear.withLinearIO (Linear.fmap (move . consume) action)
