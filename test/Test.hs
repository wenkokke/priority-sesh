module Main where


import           Test.HUnit
import qualified Test.OneShot as OneShot
import qualified Test.Session as Session
import qualified Test.Session.Priority as Priority

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
