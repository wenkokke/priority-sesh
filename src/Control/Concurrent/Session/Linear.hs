{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE InstanceSigs            #-}
{-# LANGUAGE LinearTypes             #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE RebindableSyntax        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Concurrent.Session.Linear
  -- Session types and channels
  ( Send
  , Recv
  , End
  , Session (..)
  -- Communication primitives
  , send
  , recv
  , close
  , connect
  -- Binary choice
  , Select
  , Offer
  , selectLeft
  , selectRight
  , offerEither
  ) where

import           Prelude.Linear hiding (Dual, IO)
import           Control.Exception
import           Control.Concurrent.Linear
import qualified Control.Concurrent.OneShot.Linear as OneShot
import           Control.Functor.Linear
import           Data.Bifunctor.Linear
import           Data.Functor.Linear (void)
import           Data.Kind (Type)
import           Data.Unrestricted.Linear
import qualified System.IO.Linear as Linear
import qualified Unsafe.Linear as Unsafe


-- * Session types

newtype Send a s = Send (OneShot.SendOnce (a, Dual s))
newtype Recv a s = Recv (OneShot.RecvOnce (a, s))
newtype End      = End OneShot.SyncOnce


-- * Duality and session initiation

class ( Consumable s
      , Session (Dual s)
      , Dual (Dual s) ~ s
      ) => Session s where
  type Dual s = result | result -> s
  new :: Linear.IO (s, Dual s)

instance Consumable (Send a s) where
  consume (Send ch_s) = consume ch_s

instance Consumable (Recv a s) where
  consume (Recv ch_r) = consume ch_r

instance Consumable End where
  consume (End sync) = consume sync

instance Session s => Session (Send a s) where
  type Dual (Send a s) = Recv a (Dual s)
  new = bimap Send Recv <$> OneShot.new

instance Session s => Session (Recv a s) where
  type Dual (Recv a s) = Send a (Dual s)
  new = bimap Recv Send . swap <$> OneShot.new

instance Session End where
  type Dual End = End
  new = bimap End End <$> OneShot.newSync

instance Session () where
  type Dual () = ()
  new = return ((), ())


-- * Communication primitives

send :: Session s => (a, Send a s) %1 -> Linear.IO s
send (x, Send ch_s) = do
  (here, there) <- new
  OneShot.send ch_s (x, there)
  return here

recv :: Recv a s %1 -> Linear.IO (a, s)
recv (Recv ch_r) = OneShot.recv ch_r

close :: End %1 -> Linear.IO ()
close (End sync) = OneShot.sync sync

connect :: Session s => (s %1 -> Linear.IO ()) %1 -> (Dual s %1 -> Linear.IO a) %1 -> Linear.IO a
connect k1 k2 = do (s1, s2) <- new; void $ forkIO (k1 s1); k2 s2


-- * Binary choice

type Select s1 s2 =
  Send (Either (Dual s1) (Dual s2)) ()

type Offer s1 s2 =
  Recv (Either s1 s2) ()

selectLeft :: (Session s1, Session s2) =>
  Select s1 s2 %1 ->
  Linear.IO s1
selectLeft s = do
  (here, there) <- new
  send (Left there, s)
  return here

selectRight :: (Session s1, Session s2) =>
  Select s1 s2 %1 ->
  Linear.IO s2
selectRight s = do
  (here, there) <- new
  send (Right there, s)
  return here

offerEither :: (Session s1, Session s2) =>
  Offer s1 s2 %1 ->
  (Either s1 s2 %1 -> Linear.IO a) %1 ->
  Linear.IO a
offerEither s match = do
  (e, ()) <- recv s
  match e

-- -}
-- -}
-- -}
-- -}
-- -}
