{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableSuperClasses #-}
{-# LANGUAGE RebindableSyntax #-}

module Lib where

import Prelude.Linear hiding (Dual(..))
import qualified Control.Concurrent.OneShot.Linear as OneShot
import Control.Monad.Linear


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


-- * Instances

class (Session (Dual s), Dual (Dual s) ~ s) => Session s where
  type Dual s
  new :: IO (s, Dual s)

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


send :: Send a s %1-> a %1-> IO s
send (Send sender) x = do
  (myCont, theirCont) <- new
  OneShot.send sender (x, theirCont)
  return myCont

recv :: Recv a s %1-> IO (a, s)
recv (Recv receiver) = do
  (x, myCont) <- OneShot.receive receiver
  return (x, myCont)

close :: End %1-> IO ()
close (End sender receiver) = do
  OneShot.send sender ()
  OneShot.receive receiver

{-
forkWithThreadId :: Session s => (s -> IO ()) -> IO (ThreadId, Dual s)
forkWithThreadId p = do
  (mySess, theirSess) <- new
  threadId <- forkIO (p theirSess)
  return (threadId, mySess)

fork :: Session s => (s -> IO ()) -> IO (Dual s)
fork = fmap snd . forkWithThreadId

type Offer s1 s2 = Recv (Either s1 s2) End
type Choose s1 s2 = Send (Either s1 s2) End

offerEither :: Offer s1 s2 -> (s1 -> IO a) -> (s2 -> IO a) -> IO a
offerEither chan k1 k2 = do
  (x, myCont) <- recv chan
  let _ = cancel myCont
  either k1 k2 x

-- -}
-- -}
-- -}
-- -}
-- -}
