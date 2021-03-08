{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}

module Control.Concurrent.Session.Raw.Linear.Test where

import           Control.Concurrent.Linear
import           Control.Concurrent.Session.Raw.Linear
import           Control.Functor.Linear
import           Data.Bifunctor.Linear
import           Data.Functor.Linear (void)
import qualified Data.V.Linear as V
import           Debug.Trace.Linear (traceIO)
import qualified Prelude
import           Prelude.Linear hiding (Dual, print)
import           System.IO.Linear (fromSystemIO)
import qualified System.IO.Linear as Linear
import           System.IO.Silently.Linear
import           Test.HUnit
import           Test.HUnit.Linear (assertOutput, assertBlockedIndefinitelyOnMVar)
import qualified Unsafe.Linear as Unsafe


-- * Ping

type Ping = Send () End
type Pong = Dual Ping

-- |Test sending a ping across threads.
pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert ping)
  where
    ping = do
      connect
        (\s -> do
            s <- send ((), s)
            close s
        )
        (\s -> do
            ((), s) <- recv s
            close s
        )


-- * Calculator

type NegServer n = Recv n (Send n End)
type AddServer n = Recv n (Recv n (Send n End))

type CalcServer n = Offer (NegServer n) (AddServer n)
type CalcClient n = Dual (CalcServer n)


-- |Test using the calculator server for negation.
calcWorks :: Test
calcWorks = TestLabel "calc" $ TestList
  [ TestLabel "neg" $ TestCase (assert neg)
  , TestLabel "add" $ TestCase (assert add)
  ]
  where
    -- Calculator server, which offers negation and addition.
    calcServer :: CalcServer Int %1 -> Linear.IO ()
    calcServer = offerEither match
      where
        match :: Either (NegServer Int) (AddServer Int) %1 -> Linear.IO ()
        match (Left s) = do
          (x, s) <- recv s
          s <- send (negate x, s)
          close s
        match (Right s) = do
          (x, s) <- recv s
          (y, s) <- recv s
          s <- send (x + y, s)
          close s

    -- Server offers calcuator, client chooses (negate 42).
    neg = do
      x <- connect calcServer
        (\s -> do
            s <- selectLeft s
            s <- send (42, s)
            (x, s) <- recv s
            close s
            return x
        )
      return $ move (x == -42)

    -- Server offers calculator, client chooses 4 + 5.
    add = do
      x <- connect calcServer
        (\s -> do
            s <- selectRight s
            s <- send (4, s)
            s <- send (5, s)
            (x, s) <- recv s
            close s
            return x
        )
      return $ move (x == 9)


-- * Cancellation

-- |Test the interaction of cancel with send and receive.
cancelWorks :: Test
cancelWorks = TestLabel "cancel" $ TestList
  [ TestLabel "recv" $ TestCase (assertBlockedIndefinitelyOnMVar @() cancelRecv)
  , TestLabel "send" $ TestCase (assert cancelSend)
  ]
  where
    -- Server cancels, client tries to receive.
    cancelRecv = do
      connect
        (\s -> return (consume s))
        (\s -> do ((), ()) <- recv s; return ())

    -- Server cancels, client tries to send.
    cancelSend = do
      connect
        (\s -> return (consume s))
        (\s -> do () <- send ((), s); return ())


-- * Cyclic Scheduler

newtype Out a = Out (Recv a (In a))
newtype In a = In (Send a (Out a))

instance Consumable (Out a) where
  consume (Out x) = consume x

instance Consumable (In a) where
  consume (In x) = consume x

instance Session (Out a) where
  type Dual (Out a) = In a
  new = bimap Out In <$> new

instance Session (In a) where
  type Dual (In a) = Out a
  new = bimap In Out <$> new

sched :: Out a %1 -> [In a] %1 -> Linear.IO ()
sched (Out s1) (In s2 : rest) = do
  (x, s1) <- recv s1
  s2 <- send (x, s2)
  sched s2 (rest ++ [s1])

print :: Show a => a %1 -> Linear.IO ()
print x = fromSystemIO $ Unsafe.toLinear Prelude.print x

printer :: forall a. (Dupable a, Ord a, FromInteger a, Show a) => Out a %1 -> Linear.IO ()
printer = printer0
  where
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
      printer s
    printer2 False x3 s = do
      x3 `lseq` s `lseq` return ()

add1 :: Out Int %1 -> Linear.IO ()
add1 (Out s) = do
  (x, In s) <- recv s
  s <- send (x + 1, s)
  add1 s

schedWorks :: Test
schedWorks = TestLabel "sched" $ TestCase (assertOutput "" "2\n4\n6\n8\n10\n" program)
  where
    program = do
      (In s, o1) <- new
      (i2, o2) <- new
      (i3, o3) <- new
      void $ forkIO (sched o1 [i2, i3])
      void $ forkIO (add1 o2)
      void $ forkIO (add1 o3)
      o1' <- send (0, s)
      printer o1'
