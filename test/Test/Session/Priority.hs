{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE GADTs                 #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE LinearTypes           #-}
{-# LANGUAGE NoStarIsType          #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}
{-# LANGUAGE UndecidableInstances  #-}

module Test.Session.Priority where

import           Control.Concurrent.Session.DF.Priority.Linear as Session
import           Control.Functor.Linear (Functor(..), (<$>))
import           Data.Bifunctor.Linear
import           Data.Type.Nat as Nat (Nat(..), type (+))
import           Data.Type.Priority as Priority (Priority(..), type (<), type Min, type Max)
import           Data.Void (Void)
import           Prelude.Linear hiding (Min, Max, Dual)
import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)


-- * Rebind do-notation to Sesh "monad"

fail :: String -> Sesh t p q a
fail = error

(>>) :: (q < p') =>
  Sesh t p q () %1 ->
  Sesh t p' q' b %1 ->
  Sesh t (Min p p') (Max q q') b
(>>) = (Session.>>>)

(>>=) :: (q < p') =>
  Sesh t p q a %1 ->
  (a %1 -> Sesh t p' q' b) %1 ->
  Sesh t (Min p p') (Max q q') b
(>>=) = (Session.>>>=)

return :: a %1 -> Sesh t 'Top 'Bot a
return = Session.ireturn


-- * Ping

type Ping t = Send t 0 () (End t 1)
type Pong t = Dual (Ping t)

pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert (runSeshIO main))
  where
    main = connect ping pong

    ping :: Ping t %1 -> _
    ping s = do
      s <- send ((), s)
      close s

    pong :: Pong t %1 -> _
    pong s = do
      ((), s) <- recv s
      close s


-- * Calculator

type NegServer t = Recv t 1 Int (Send t 2 Int (End t 4))
type AddServer t = Recv t 1 Int (Recv t 2 Int (Send t 3 Int (End t 4)))

type CalcServer t = Offer t 0 (NegServer t) (AddServer t)
type CalcClient t = Dual (CalcServer t)

-- |Test using the calculator server for negation.
calcWorks :: Test
calcWorks = TestLabel "calc" $ TestList
  [ TestLabel "neg" $ TestCase (assert (runSeshIO negMain))
  , TestLabel "add" $ TestCase (assert (runSeshIO addMain))
  ]
  where
    negMain = connect calcServer negClient
    addMain = connect calcServer addClient

    -- Calculator server, which offers negation and addition.
    calcServer :: CalcServer t %1 -> _
    calcServer s = offerEither s match
      where
        match :: Either (NegServer t) (AddServer t) %1 -> _
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
    negClient :: CalcClient t %1 -> _
    negClient s = do
      s <- selectLeft s
      s <- send (42, s)
      (x, s) <- recv s
      close s
      return $ x == -42

    -- Server offers calculator, client chooses 4 + 5.
    addClient :: CalcClient t %1 -> _
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
  [ TestLabel "recv" $ TestCase (assertBlockedIndefinitelyOnMVar @() (runSeshIO cancelAndRecv))
  , TestLabel "send" $ TestCase (assert (runSeshIO cancelAndSend))
  ]
  where
    -- Server cancels, client tries to receive.
    cancelAndRecv = do
      (s, s') <- new
      fork $ return (consume s')
      ((), ()) <- recv @0 s
      return ()

    -- Server cancels, client tries to send.
    cancelAndSend = do
      (s, s') <- new
      fork $ return (consume s')
      () <- send @0 ((), s)
      return ()


-- * Deadlock (does not compile, rightfully)

-- deadlockFails :: Test
-- deadlockFails = TestLabel "deadlock" $ TestCase (assert (runSeshIO woops))
--   where
--     woops :: Sesh t _ _ ()
--     woops = do
--       (s1, r1) <- new :: Sesh t _ _ (Send t _ Void (), Recv t _ Void ())
--       (s2, r2) <- new :: Sesh t _ _ (Send t _ Void (), Recv t _ Void ())
--       fork $ do (void, ()) <- recv @0 r1
--                 send @1 (void, s2)
--       (void, ()) <- recv @0 r2
--       send @1 (void, s1)


-- * Cyclic scheduler

type SR t o1 o2 a = Send t o1 a (Recv t o2 a ())
type RS t o1 o2 a = Dual (SR t o1 o2 a)

sched4 ::
  RS t 0 7 a %1 ->
  SR t 1 2 a %1 ->
  SR t 3 4 a %1 ->
  SR t 5 6 a %1 ->
  Sesh t ('Val 0) ('Val 7) ()
sched4 s1 s2 s3 s4 = do
  (x, s1) <- recv s1
  s2 <- send (x, s2)
  (x, ()) <- recv s2
  s3 <- send (x, s3)
  (x, ()) <- recv s3
  s4 <- send (x, s4)
  (x, ()) <- recv s4
  send (x, s1)

adder ::
  ('Val o1 < 'Val o2) =>
  RS t o1 o2 Int %1 ->
  Sesh t ('Val o1) ('Val o2) ()
adder s = do
  (x, s) <- recv s
  send (x + 1, s)

main ::
  ('Val o1 < 'Val o2) =>
  Int %1 ->
  Int %1 ->
  SR t o1 o2 Int %1 ->
  Sesh t ('Val o1) ('Val o2) Bool
main x y s = do
  s <- send (x, s)
  (x, ()) <- recv s
  return $ x == y

schedWorks :: Test
schedWorks = TestLabel "sched" $ TestCase (assert (runSeshIO conf))
  where
    conf = do
      (sr1, rs1) <- new
      (sr2, rs2) <- new
      (sr3, rs3) <- new
      (sr4, rs4) <- new
      fork $ sched4 rs1 sr2 sr3 sr4
      fork $ adder rs2
      fork $ adder rs3
      fork $ adder rs4
      main 0 3 sr1


-- * Counterexample for soundness :(

woops :: Sesh t ('Val 0) ('Val 0) ()
woops = do
  (s1, r1) <- new
  (s2, r2) <- new
  fork $ do
    (v, ()) <- recv r1
    fork $ send (v, s2)
  (v, ()) <- recv r2
  fork $ send (v, s1)

-- -}
-- -}
-- -}
-- -}
-- -}
