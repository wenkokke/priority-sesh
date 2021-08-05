{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE GeneralisedNewtypeDeriving #-}

module Test.Session where

import Prelude qualified
import Prelude.Linear hiding (Dual)
import Control.Concurrent.Linear
import Control.Concurrent.Session.Linear
import Control.Functor.Linear
import Data.Bifunctor.Linear
import Data.Functor.Linear (void)
import Data.V.Linear qualified as V
import Debug.Trace.Linear (traceIO)
import System.IO.Linear (fromSystemIO)
import System.IO.Linear qualified as Linear
import Test.HUnit
import Test.HUnit.Linear (assertOutput, assertBlockedIndefinitelyOnMVar)
import Unsafe.Linear qualified as Unsafe


-- * Ping

type Ping = Send () End
type Pong = Dual Ping

-- |Test sending a ping across threads.
pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert main)
  where
    main = connect ping pong

    ping :: Ping %1 -> Linear.IO ()
    ping s = do
      s <- send ((), s)
      close s

    pong :: Pong %1 -> Linear.IO ()
    pong s = do
      ((), s) <- recv s
      close s


-- * Calculator

type NegServer = Recv Int (Send Int End)
type AddServer = Recv Int (Recv Int (Send Int End))

type CalcServer = Offer NegServer AddServer
type CalcClient = Dual CalcServer

-- |Test using the calculator server for negation.
calcWorks :: Test
calcWorks = TestLabel "calc" $ TestList
  [ TestLabel "neg" $ TestCase (assert negMain)
  , TestLabel "add" $ TestCase (assert addMain)
  ]
  where
    negMain = connect calcServer negClient
    addMain = connect calcServer addClient

    -- Calculator server which offers negation and addition.
    calcServer :: CalcServer %1 -> Linear.IO ()
    calcServer s = offerEither s match
      where
        match :: Either NegServer AddServer %1 -> Linear.IO ()
        match (Left s)  = do
          (x, s) <- recv s
          s <- send (negate x, s)
          close s
        match (Right s) = do
          (x, s) <- recv s
          (y, s) <- recv s
          s <- send (x + y, s)
          close s

    -- Calculator client which chooses (negate 42).
    negClient :: CalcClient %1 -> Linear.IO Bool
    negClient s = do
      s <- selectLeft s
      s <- send (42, s)
      (x, s) <- recv s
      close s
      return $ x == -42

    -- Calculator client which chooses (4 + 5).
    addClient :: CalcClient %1 -> Linear.IO Bool
    addClient s = do
      s <- selectRight s
      s <- send (4, s)
      s <- send (5, s)
      (x, s) <- recv s
      close s
      return $ x == 9


-- * Cancellation

-- |Test the interaction of cancel with send and receive.
cancelWorks :: Test
cancelWorks = TestLabel "cancel" $ TestList
  [ TestLabel "recv" $ TestCase (assertBlockedIndefinitelyOnMVar @() cancelAndRecv)
  , TestLabel "send" $ TestCase (assert cancelAndSend)
  ]
  where
    -- Server cancels, client tries to receive.
    cancelAndRecv = do
      connect
        (\s -> cancel s)
        (\s -> do ((), ()) <- recv s; return ())

    -- Server cancels, client tries to send.
    cancelAndSend = do
      connect
        (\s -> cancel s)
        (\s -> do () <- send ((), s); return ())


-- * Recursion

newtype SumServer
  = SumServer (Offer (Recv Int SumServer) (Send Int End))
  deriving Consumable

newtype SumClient
  = SumClient (Select (Send Int SumClient) (Recv Int End))
  deriving Consumable

instance Session SumServer where
  type Dual SumServer = SumClient
  new = bimap SumServer SumClient <$> new

instance Session SumClient where
  type Dual SumClient = SumServer
  new = bimap SumClient SumServer <$> new

sumWorks :: Test
sumWorks = TestLabel "sum" $ TestCase (assert main)
  where
    main = connect (sumServer 0) sumClient

    -- Server which offers summation.
    sumServer :: Int %1 -> SumServer %1 -> Linear.IO ()
    sumServer tot (SumServer s) = offerEither s (match tot)
      where
        match :: Int %1 -> Either (Recv Int SumServer) (Send Int End) %1 -> Linear.IO ()
        match tot (Left s) = do
          (x, s) <- recv s
          sumServer (tot + x) s
        match tot (Right s) = do
          s <- send (tot, s)
          close s

    -- Client which sums [1..6].
    sumClient :: SumClient %1 -> Linear.IO Bool
    sumClient (SumClient s) = do
      s <- selectLeft s
      SumClient s <- send (1, s)
      s <- selectLeft s
      SumClient s <- send (2, s)
      s <- selectLeft s
      SumClient s <- send (3, s)
      s <- selectLeft s
      SumClient s <- send (4, s)
      s <- selectLeft s
      SumClient s <- send (5, s)
      s <- selectLeft s
      SumClient s <- send (6, s)
      s <- selectRight s
      (tot, s) <- recv s
      close s
      return $ tot == 21

-- * Cyclic Scheduler

newtype Out a = Out (Recv a (In a))
  deriving Consumable

newtype In a = In (Send a (Out a))
  deriving Consumable

instance Consumable a => Session (Out a) where
  type Dual (Out a) = In a
  new = bimap Out In <$> new

instance Consumable a => Session (In a) where
  type Dual (In a) = Out a
  new = bimap In Out <$> new

schedNode :: Consumable a => Out a %1 -> [In a] %1 -> Linear.IO ()
schedNode (Out s1) (In s2 : rest) = do
  (x, s1) <- recv s1
  s2 <- send (x, s2)
  schedNode s2 (rest ++ [s1])

printNode :: forall a. (Consumable a, Dupable a, Ord a, FromInteger a, Show a) => Out a %1 -> Linear.IO ()
printNode = printer0
  where
    print :: Show a => a %1 -> Linear.IO ()
    print x = fromSystemIO $ Unsafe.toLinear Prelude.print x

    printer0 :: Out a %1 -> Linear.IO ()
    printer0 (Out s) = do
      (x, In s) <- recv s
      printer1 (dup3 x) s

    printer1 :: (a, a, a) %1 -> Send a (Out a) %1 -> Linear.IO ()
    printer1 (x1, x2, x3) s = do
      print x1
      printer2 (x2 < 10) x3 s

    printer2 :: Bool %1 -> a %1 -> Send a (Out a) %1 -> Linear.IO ()
    printer2 True  x3 s = do
      s <- send (x3, s)
      printer0 s
    printer2 False x3 s = do
      cancel x3
      cancel s
      return ()

add1Node :: Out Int %1 -> Linear.IO ()
add1Node (Out s) = do
  (x, In s) <- recv s
  s <- send (x + 1, s)
  add1Node s

schedWorks :: Test
schedWorks = TestLabel "sched" $ TestCase (assertOutput "" "2\n4\n6\n8\n10\n" main)
  where
    main = do
      (In s, o1) <- new
      (i2, o2) <- new
      (i3, o3) <- new
      void $ forkIO (schedNode o1 [i2, i3])
      void $ forkIO (add1Node o2)
      void $ forkIO (add1Node o3)
      o1' <- send (0, s)
      printNode o1'

-- -}
-- -}
-- -}
-- -}
-- -}
