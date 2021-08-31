{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeApplications #-}

module Test.OneShot where

import Control.Concurrent.Channel.OneShot (CommunicationException (..), new, recv, send)
import Control.Concurrent.Linear (forkIO_)
import Control.Functor.Linear (Monad (..), return)
import Data.Functor.Linear (void)
import Data.Proxy (Proxy (..))
import Prelude.Linear
import System.IO.Linear qualified as Linear
import System.IO.Linear.Cancelable (Cancelable (..))
import Test.HUnit (Assertion, Test (..), assert)
import Test.HUnit.Linear (assertException)

pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert ping)
  where
    ping = do
      (sender, receiver) <- new
      forkIO_ (send sender ())
      recv receiver

cancelWorks :: Test
cancelWorks =
  TestLabel "cancel" $
    TestList
      [ TestLabel "recv" $ TestCase (assertException (Proxy @CommunicationException) cancelAndRecv),
        TestLabel "send" $ TestCase (assert cancelAndSend)
      ]
  where
    -- Server cancels, client tries to receive.
    cancelAndRecv :: Linear.IO ()
    cancelAndRecv = do
      (sender, receiver) <- new
      cancel sender
      recv receiver

    -- Server cancels, client tries to send.
    cancelAndSend :: Linear.IO ()
    cancelAndSend = do
      (sender, receiver) <- new
      cancel receiver
      send sender ()
