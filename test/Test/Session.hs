{-# LANGUAGE LambdaCase       #-}
{-# LANGUAGE TypeApplications #-}

module Test.Session where

import Prelude.Linear              hiding (Dual)
import Control.Concurrent.Channel.Session
import Control.Concurrent.Linear   (forkIO_)
import Control.Functor.Linear      (Monad(..), return)
import Data.Proxy                  (Proxy(..))
import System.IO.Linear            qualified as Linear
import System.IO.Linear.Cancelable (Cancelable(..))
import Test.HUnit                  (Test(..), Assertable(..), Assertion)
import Test.HUnit.Linear           (assertOutput, assertException)
import Unsafe.Linear               qualified as Unsafe


-- * Ping

-- |Test sending a ping across threads.
pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert main)
  where
    main :: Linear.IO ()
    main = do
      s <- connect $ \s -> do
        s <- send ((), s)
        close s
      ((), s) <- recv s
      close s


-- * Calculator Server

type CalcServer = Recv CalcOp ()
type CalcClient = Dual CalcServer

data CalcOp
  = Neg (Recv Int (Send Int End))
  | Add (Recv Int (Recv Int (Send Int End)))

instance Consumable CalcOp where
  consume (Neg s) = consume s
  consume (Add s) = consume s


-- | The calculator server offers a choice between various calculations.
calcServer :: CalcServer %1 -> Linear.IO ()
calcServer s = offer s $ \case

  -- Offer negation:
  Neg s -> do
    (x, s) <- recv s
    s <- send (negate x, s)
    close s

  -- Offer addition:
  Add s -> do
    (x, s) <- recv s
    (y, s) <- recv s
    s <- send (x + y, s)
    close s

-- | The negation client uses the negation action of the calculator server.
negClient :: CalcClient %1 -> Linear.IO Bool
negClient s = do
  s <- select Neg s
  s <- send (42, s)
  (r, s) <- recv s
  close s
  return (r == -42)

-- | The addition client uses the addition action of the calculator server.
addClient :: CalcClient %1 -> Linear.IO Bool
addClient s = do
  s <- select Add s
  s <- send (4, s)
  s <- send (5, s)
  (r, s) <- recv s
  close s
  return (r == 9)

calcWorks :: Test
calcWorks = TestLabel "calc" $ TestList
  [ TestLabel "neg" $ TestCase (assert negMain)
  , TestLabel "add" $ TestCase (assert addMain)
  ]
  where
    negMain = connect calcServer >>= negClient
    addMain = connect calcServer >>= addClient


-- * Cancelation

-- |Test the interaction of cancel with send and receive.
cancelWorks :: Test
cancelWorks = TestLabel "cancel" $ TestList
  [ TestLabel "recv" $ TestCase (assertException (Proxy @CommunicationException) cancelAndRecv)
  , TestLabel "send" $ TestCase (assert cancelAndSend)
  ]
  where
    -- Server cancels, client tries to receive.
    cancelAndRecv :: Linear.IO ()
    cancelAndRecv = do
      s <- connect cancel
      ((), ()) <- recv s
      return ()

    -- Server cancels, client tries to send.
    cancelAndSend = do
      s <- connect cancel
      () <- send ((), s)
      return ()


-- * Summation server

type SumServer = Recv SumOp ()
type SumClient = Dual SumServer

data SumOp
  = More (Recv Int SumServer)
  | Done (Send Int End)

instance Consumable SumOp where
  consume (More s) = consume s
  consume (Done s) = consume s

-- | Summation server receives numbers and adds them until the 'Done' message is
--   received, at which point it sends back the sum.
sumServer :: Int %1 -> SumServer %1 -> Linear.IO ()
sumServer tot s = offer s $ \case

  -- More numbers:
  More s -> do
    (x, s) <- recv s
    sumServer (tot + x) s

  -- Send result:
  Done s -> do
    s <- send (tot, s)
    close s

-- Client which sums [1..6].
sumClient :: SumClient %1 -> Linear.IO Bool
sumClient s = do
  s <- select More s
  s <- send (1, s)
  s <- select More s
  s <- send (2, s)
  s <- select More s
  s <- send (3, s)
  s <- select More s
  s <- send (4, s)
  s <- select More s
  s <- send (5, s)
  s <- select More s
  s <- send (6, s)
  s <- select Done s
  (tot, s) <- recv s
  close s
  return (tot == 21)

-- | Test the summation server.
sumWorks :: Test
sumWorks = TestLabel "sum" $ TestCase (assert main)
  where
    main = connect (sumServer 0) >>= sumClient


-- * Rebindable Syntax

fail = error
