{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Test.OneShot where

import           Control.Concurrent.Linear
import           Control.Concurrent.OneShot.Linear
import           Control.Functor.Linear
import           Data.Functor.Linear (void)
import           Prelude.Linear hiding (Dual)
import qualified System.IO.Linear as Linear
import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)


pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert ping)
  where
    ping = do
      (chan_s, chan_r) <- new
      void $ forkIO (send chan_s ())
      recv chan_r


cancelWorks :: Test
cancelWorks = TestLabel "cancel" $ TestList
  [ TestLabel "recv" $ TestCase (assertBlockedIndefinitelyOnMVar @() cancelRecv)
  , TestLabel "send" $ TestCase (assert cancelSend)
  ]
  where
    -- Server cancels, client tries to receive.
    cancelRecv = do
      (chan_s, chan_r) <- new
      void $ forkIO (return (consume chan_s))
      recv chan_r

    -- Server cancels, client tries to send.
    cancelSend = do
      (chan_s, chan_r) <- new
      void $ forkIO (return (consume chan_r))
      send chan_s ()
