{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Control.Concurrent.Session.Linear
  ( Session(Dual)
  , Send
  , Recv
  , End
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
  type Dual s
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

send :: Send a s %1-> a %1-> Linear.IO s
send (Send sender) x = do
  (mySess, theirSess) <- new
  OneShot.send sender (x, theirSess)
  return mySess

recv :: Recv a s %1-> Linear.IO (a, s)
recv (Recv receiver) = do
  (x, mySess) <- OneShot.receive receiver
  return (x, mySess)

close :: End %1-> Linear.IO ()
close (End sender receiver) = do
  OneShot.send sender ()
  OneShot.receive receiver

connect :: Session s =>
           (s %1-> Linear.IO ()) ->
           (Dual s %1-> Linear.IO ()) ->
           Linear.IO ()
connect p1 p2 = do
  (theirSess, mySess) <- new
  consume <$> forkLinearIO (p1 theirSess)
  p2 mySess
