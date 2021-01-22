{-# OPTIONS -fno-warn-partial-type-signatures #-}
{-# LANGUAGE DataKinds             #-}
{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE LinearTypes           #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE RebindableSyntax      #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeApplications      #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE TypeFamilies          #-}

module Control.Concurrent.Session.Linear.Test where

import           Test.HUnit
import           Test.HUnit.Linear (assertBlockedIndefinitelyOnMVar)
import qualified Prelude
import           Prelude.Linear hiding (Min, Max, Dual, Monad(..))
import           Control.Concurrent.Session.Linear as Session
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe


-- * Rebind do-notation to Sesh "monad"

fail :: String -> Sesh t p q a
fail = error

(>>) :: (Consumable a, q <= p') =>
  Sesh t p q a %1 ->
  Sesh t p' q' b %1 ->
  Sesh t (Min p p') (Max q q') b
mx >> my = mx Session.>>>= \x -> x `lseq` my

(>>=) :: (q <= p') =>
  Sesh t p q a %1 ->
  (a %1 -> Sesh t p' q' b) %1 ->
  Sesh t (Min p p') (Max q q') b
(>>=) = (Session.>>>=)

return :: a %1 -> Sesh t (Pr a) 'Bot a
return = Session.ireturn


-- * Ping

pingWorks :: Test
pingWorks = TestLabel "ping" $ TestCase (assert (runSesh ping))
  where
    ping :: Sesh t _ _ (Ur ())
    ping = withNew (\(s1, s2) -> do
      spawn $ do
        s1 <- send @0 ((), s1)
        close @1 s1
      ((), s2) <- recv s2
      close s2
      return $ Ur ())
