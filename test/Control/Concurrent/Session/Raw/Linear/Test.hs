{-# OPTIONS_GHC -fno-warn-name-shadowing #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE LinearTypes         #-}
{-# LANGUAGE RebindableSyntax    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}

module Control.Concurrent.Session.Raw.Linear.Test where

import           Control.Concurrent.Session.Raw.Linear
import           Control.Functor.Linear
import qualified Prelude
import           Prelude.Linear hiding (Dual)
import qualified System.IO.Linear as Linear
import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)
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
      connect cancel
        (\s -> do
            ((), s) <- recv s
            close s
        )

    -- Server cancels, client tries to send.
    cancelSend = do
      connect cancel
        (\s -> do
            s <- send ((), s)
            cancel (s :: End) -- close tries to sync
        )
