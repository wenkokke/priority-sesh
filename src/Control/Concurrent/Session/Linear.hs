{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeFamilyDependencies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Concurrent.Session.Linear
  ( Session(Dual)
  , connect
  , Send
  , Recv
  , End
  , send
  , recv
  , close
  , cancel
  , Select
  , Offer
  , selectLeft
  , selectRight
  , offerEither
  ) where

import           Prelude.Linear hiding (Dual)
import           Control.Concurrent.Linear
import qualified Control.Concurrent.OneShot.Linear as OneShot
import           Control.Monad.Linear
import           Data.Bifunctor.Linear
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe



-- * Session types

data Send a s where
  Send :: Session s =>
          OneShot.Sender (a, Dual s) %1->
          Send a s

data Recv a s where
  Recv :: Session s =>
          OneShot.Receiver (a, s) %1->
          Recv a s

data End where
  End :: OneShot.Sender () %1->
         OneShot.Receiver () %1->
         End


-- * Duality and session initiation

class (Session (Dual s), Dual (Dual s) ~ s) => Session s where
  type Dual s = result | result -> s
  new :: Linear.IO (s, Dual s)

instance Session s => Session (Send a s) where
  type Dual (Send a s) = Recv a (Dual s)
  new = do
    (sender, receiver) <- OneShot.new
    return (Send sender, Recv receiver)

instance Session s => Session (Recv a s) where
  type Dual (Recv a s) = Send a (Dual s)
  new = do
    (sender, receiver) <- OneShot.new
    return (Recv receiver, Send sender)

instance Session End where
  type Dual End = End
  new = do
    (sender1, receiver1) <- OneShot.new
    (sender2, receiver2) <- OneShot.new
    return (End sender1 receiver2, End sender2 receiver1)


-- * Communication primitives

send :: a %1-> Send a s %1->
        Linear.IO s
send x (Send sender) = do
  (here, there) <- new
  OneShot.send sender (x, there)
  return here

recv :: Recv a s %1->
        Linear.IO (a, s)
recv (Recv receiver) = do
  (x, here) <- OneShot.receive receiver
  return (x, here)

close :: End %1->
         Linear.IO ()
close (End sender receiver) = do
  OneShot.send sender ()
  OneShot.receive receiver

cancel :: s %1-> Linear.IO ()
cancel s = return $ Unsafe.toLinear2 const () s

connect :: Session s =>
           (s %1-> Linear.IO ()) ->
           (Dual s %1-> Linear.IO a) ->
           Linear.IO a
connect p1 p2 = do
  (here, there) <- new
  consume <$> forkLinearIO (p1 there)
  p2 here


-- * Binary choice

type Select s1 s2 = Send (Either (Dual s1) (Dual s2)) End

selectLeft :: (Session s1, Session s2) =>
              Select s1 s2 %1->
              Linear.IO s1
selectLeft s = do
  (here, there) <- new
  s' <- send (Left there) s
  cancel s'
  return here

selectRight :: (Session s1, Session s2) =>
               Select s1 s2 %1->
               Linear.IO s2
selectRight s = do
  (here, there) <- new
  s' <- send (Right there) s
  cancel s'
  return here


type Offer s1 s2 = Recv (Either s1 s2) End

offerEither :: (Session s1, Session s2) =>
               (Either s1 s2 %1-> Linear.IO a) ->
               Offer s1 s2 %1->
               Linear.IO a
offerEither match s = do
  (e, s') <- recv s
  cancel s'
  match e
