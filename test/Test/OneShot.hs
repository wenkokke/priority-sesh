{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE TypeApplications    #-}

module Test.OneShot where
import Prelude.Linear
import Control.Concurrent.Linear          (forkIO_)
import Control.Concurrent.Channel.OneShot (CommunicationException(..), new, send, recv)
import Control.Functor.Linear             (Monad(..), return)
import Data.Proxy                         (Proxy(..))
import Data.Functor.Linear                (void)
import System.IO.Linear                   qualified as Linear
import System.IO.Linear.Cancelable        (Cancelable(..))
import Test.HUnit                         (Test(..), Assertion, assert)
import Test.HUnit.Linear                  (assertException)


pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert ping)
  where
    ping = do
      (sender, receiver) <- new
      forkIO_ (send sender ())
      recv receiver


cancelWorks :: Test
cancelWorks = TestLabel "cancel" $ TestList
  [ TestLabel "recv" $ TestCase (assertException (Proxy @CommunicationException) cancelAndRecv)
  , TestLabel "send" $ TestCase (assert cancelAndSend)
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
