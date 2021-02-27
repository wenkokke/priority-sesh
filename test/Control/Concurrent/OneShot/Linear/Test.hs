{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Control.Concurrent.OneShot.Linear.Test where

import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)
import qualified Prelude
import           Prelude.Linear hiding (Dual)
import           Control.Concurrent.Linear
import           Control.Concurrent.OneShot.Linear
import           Control.Monad.Linear
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe


pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert ping)
  where
    ping :: Linear.IO (Ur ())
    ping = do
      (chan_s, chan_r) <- new
      () <- fmap consume . forkLinearIO $ do
        send chan_s ()
      () <- recv chan_r
      return $ Ur ()


cancelWorks :: Test
cancelWorks = TestLabel "cancel" $ TestList
  [ TestLabel "recv" $ TestCase (assertBlockedIndefinitelyOnMVar cancelRecv)
  , TestLabel "send" $ TestCase (assert cancelSend)
  ]
  where
    spawn :: Linear.IO () %1 -> Linear.IO ()
    spawn = fmap consume . forkLinearIO

    -- Server cancels, client tries to receive.
    cancelRecv = do
      (chan_s, chan_r) <- new
      spawn $ return (consume chan_s)
      recv chan_r
      return $ Ur ()

    -- Server cancels, client tries to send.
    cancelSend = do
      (chan_s, chan_r) <- new
      spawn $ return (consume chan_r)
      send chan_s ()
      return $ Ur ()

