module Main where


import           Control.Concurrent.Session.DF.Priority.Linear
import qualified Control.Functor.Linear as Linear
import           Data.Unrestricted.Linear (Movable(..))
import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)
import qualified Test.OneShot as OneShot
import qualified Test.Session as Session
import qualified Test.Session.Priority as Priority
import           System.IO.Linear (withLinearIO)

main :: IO ()
main = withLinearIO (Linear.fmap move (runSeshIO Priority.woops))

{-
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
      , Priority.woopsWorks  -- Counterexample for soundness :(
      ]

-- -}
-- -}
-- -}
-- -}
-- -}
