module Main where

import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)
import qualified Control.Concurrent.OneShot.Linear.Test as OneShot
import qualified Control.Concurrent.Session.Raw.Linear.Test as Raw
import qualified Control.Concurrent.Session.Linear.Test as Session

main :: IO Counts
main = runTestTT tests
  where
    tests :: Test
    tests = TestList
      [ OneShot.pingWorks    -- Basic.
      , OneShot.cancelWorks  -- Cancellation.
      , Raw.pingWorks        -- Basic.
      , Raw.calcWorks        -- Choice.
      , Raw.cancelWorks      -- Cancellation.
      , Raw.sumWorks         -- Recursion.
      , Raw.schedWorks       -- Cyclic structure.
      , Session.pingWorks    -- Basic.
      , Session.calcWorks    -- Choice.
      , Session.cancelWorks  -- Cancellation.
      ]
