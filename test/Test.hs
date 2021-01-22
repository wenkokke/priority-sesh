module Main where

import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)
import qualified Control.Concurrent.Session.Raw.Linear.Test as Raw
import qualified Control.Concurrent.Session.Linear.Test as Session

main :: IO Counts
main = runTestTT tests
  where
    tests :: Test
    tests = TestList
      [ Raw.pingWorks
      , Raw.calcWorks
      , Raw.cancelWorks
      , Session.pingWorks
      ]


