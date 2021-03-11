{-# OPTIONS_GHC -fno-warn-partial-type-signatures #-}
{-# OPTIONS_GHC -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE LinearTypes #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Session.Priority where

import           Control.Concurrent.Session.DF.Priority.Linear as Session
import           Control.Functor.Linear (Functor(..), (<$>))
import           Data.Bifunctor.Linear
import           Data.Type.Nat as Nat (Nat(..), type (+))
import           Data.Type.Priority as Priority (Priority(..), type (<), type Min, type Max)
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
  [ TestLabel "recv" $ TestCase (assertBlockedIndefinitelyOnMVar @() (runSeshIO cancelRecv))
  , TestLabel "send" $ TestCase (assert (runSeshIO cancelSend))
  ]
  where
    -- Server cancels, client tries to receive.
    cancelRecv = do
      (s, s') <- new
      fork $ return (consume s')
      ((), ()) <- recv @0 s
      return ()

    -- Server cancels, client tries to send.
    cancelSend = do
      (s, s') <- new
      fork $ return (consume s')
      () <- send @0 ((), s)
      return ()


-- * Deadlock (does not compile, rightfully)

-- deadlockFails :: Test
-- deadlockFails = TestLabel "deadlock" $ TestCase (assert (runSeshIO deadlock))
--   where
--     deadlock :: Sesh t 'Top ('Val 3) ()
--     deadlock = do
--       (s1, r1) <- new
--       (s2, r2) <- new
--       fork $ do ((), r1) <- recv @0 r1
--                  close @1 r1
--                  s2 <- send @2 ((), s2)
--                  close @3 s2
--       ((), r2) <- recv @2 r2
--       close @3 r2
--       s1 <- send @0 ((), s1)
--       close @1 s1


-- * Pipe (does not compile, wrongfully)

-- newtype In t o = In (Recv t o Int (In t (2 + o)))
-- newtype Out t o = Out (Send t o Int (Out t (2 + o)))
--
-- instance Consumable (In t o) where
--   consume (In s) = consume s
--
-- instance Consumable (Out t o) where
--   consume (Out s) = consume s
--
-- instance Session (In t o) where
--   type Dual (In t o) = Out t o
--   new = bimap In Out <$> new
--
-- instance Session (Out t o) where
--   type Dual (Out t o) = In t o
--   new = bimap Out In <$> new
--
-- pipe :: forall o t. In t o %1 -> Out t (1 + o) %1 -> Sesh t ('Val o) 'Top ()
-- pipe (In ch_r) (Out ch_s) = do
--   (x, ch_r) <- recv @o ch_r
--   ch_s <- send @(1 + o) (x, ch_s)
--   pipe @(2 + o) ch_r ch_s

-- -}
-- -}
-- -}
-- -}
-- -}
