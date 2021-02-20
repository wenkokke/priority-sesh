{-# OPTIONS -fno-warn-partial-type-signatures #-}
{-# OPTIONS -fno-warn-name-shadowing #-}

{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LambdaCase            #-}
{-# LANGUAGE LinearTypes           #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Concurrent.Session.Linear.Test where

import           Control.Concurrent.Session.Linear as Session
import           Data.Void (Void)
import qualified Prelude
import           Prelude.Linear hiding (Min, Max, Dual, Monad(..))
import qualified System.IO.Linear as Linear
import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)
import qualified Unsafe.Linear as Unsafe


-- * Rebind do-notation to Sesh "monad"

fail :: String -> Sesh t p q a
fail = error

(>>) :: (Consumable a, q < p') =>
  Sesh t p q a %1 ->
  Sesh t p' q' b %1 ->
  Sesh t (Min p p') (Max q q') b
mx >> my = mx Session.>>>= \x -> x `lseq` my

(>>=) :: (q < p') =
  >
  Sesh t p q a %1 ->
  (a %1 -> Sesh t p' q' b) %1 ->
  Sesh t (Min p p') (Max q q') b
(>>=) = (Session.>>>=)

return :: a %1 -> Sesh t (Pr a) 'Bot a
return = Session.ireturn


-- * Ping

pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert (runSeshIO ping))
  where
    ping :: Sesh t _ _ (Ur ())
    ping = withNew (\(s1, s2) -> do
      spawn $ do
        s1 <- send @0 ((), s1)
        close @1 s1
      ((), s2) <- recv s2
      close s2
      return $ Ur ())


-- * Calculator

type NegServer t = Recv t 1 Int (Send t 2 Int (End t 4))
type AddServer t = Recv t 1 Int (Recv t 2 Int (Send t 3 Int (End t 4)))

type CalcServer t = Offer t 0 (NegServer t) (AddServer t)
type CalcClient t = Dual (CalcServer t)

-- |Test using the calculator server for negation.
calcWorks :: Test
calcWorks = TestLabel "calc" $ TestList
  [ TestLabel "neg" $ TestCase (assert (runSeshIO neg))
  , TestLabel "add" $ TestCase (assert (runSeshIO add))
  ]
  where
    -- Calculator server, which offers negation and addition.
    calcServer :: CalcServer t %1 -> Sesh t _ _ ()
    calcServer = offerEither (\case
                                 (Left  s) -> do (x, s) <- recv s
                                                 s <- send (negate x, s)
                                                 close s
                                 (Right s) -> do (x, s) <- recv s
                                                 (y, s) <- recv s
                                                 s <- send (x + y, s)
                                                 close s)

    -- Server offers calcuator, client chooses (negate 42).
    neg :: Sesh t _ _ (Ur Bool)
    neg = withNew (\(s, s') -> do
                      spawn (calcServer s')
                      s <- selectLeft s
                      s <- send (42, s)
                      (x, s) <- recv s
                      close s
                      return $ move (x == -42))

    -- Server offers calculator, client chooses 4 + 5.
    add :: Sesh t _ _ (Ur Bool)
    add = withNew (\(s, s') -> do
                      spawn (calcServer s')
                      s <- selectRight s
                      s <- send (4, s)
                      s <- send (5, s)
                      (x, s) <- recv s
                      close s
                      return $ move (x == 9))


-- * Cancellation

-- |Test the interaction of cancel with send and receive.
cancelWorks :: Test
cancelWorks = TestLabel "cancel" $ TestList
  [ TestLabel "recv" $ TestCase (assertBlockedIndefinitelyOnMVar (runSeshIO cancelRecv))
  , TestLabel "send" $ TestCase (assert (runSeshIO cancelSend))
  ]
  where
    -- Server cancels, client tries to receive.
    cancelRecv :: Sesh t _ _ (Ur ())
    cancelRecv = withNew (\(s, s') -> do
                             spawn (cancel s')
                             ((), s) <- recv @0 s
                             close @1 s
                             return $ Ur ())

    -- Server cancels, client tries to send.
    cancelSend :: Sesh t _ _ (Ur ())
    cancelSend = withNew (\(s, s') -> do
                             spawn (cancel s')
                             s <- send @0 ((), s)
                             close @1 s
                             return $ Ur ())

-- * Deadlock (does not compile)

-- deadlockFails :: Test
-- deadlockFails = TestLabel "deadlock" $ TestCase (assert (runSeshIO deadlock))
--   where
--     deadlock :: Sesh t 'Top ('Val 3) (Ur ())
--     deadlock =
--       withNew (\(s1, r1) ->
--         withNew (\(s2, r2) -> do
--
--           spawn $ do ((), r1) <- recv @0 r1
--                      close @1 r1
--                      s2 <- send @2 ((), s2)
--                      close @3 s2
--
--           ((), r2) <- recv @2 r2
--           close @3 r2
--           s1 <- send @0 ((), s1)
--           close @1 s1
--
--           return $ Ur ()
--         ))

-- -}
-- -}
-- -}
-- -}
-- -}
