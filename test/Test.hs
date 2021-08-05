module Main where

import Prelude
import Test.HUnit
import Test.OneShot qualified as OneShot
import Test.Session qualified as Session
import Test.Priority qualified as Priority

main :: IO Counts
main = runTestTT tests
  where
    tests :: Test
    tests = TestList
      [ OneShot.pingWorks    -- Basic.
      , OneShot.cancelWorks  -- Cancellation.
      , Session.pingWorks    -- Basic.
      , Session.calcWorks    -- Choice.
      , Session.cancelWorks  -- Cancellation.
      , Session.sumWorks     -- Recursion.
      , Session.schedWorks   -- Cyclic structure (recursive).
      , Priority.pingWorks   -- Basic.
      , Priority.calcWorks   -- Choice.
      , Priority.cancelWorks -- Cancellation.
      , Priority.schedWorks  -- Cyclic structure (non-recursive).
      ]

-- -}
-- -}
-- -}
-- -}
-- -}
