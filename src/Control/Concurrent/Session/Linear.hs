{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE LinearTypes             #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE RebindableSyntax        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}

module Control.Concurrent.Session.Linear
  -- Priorities
  ( Priority (..)
  , type (<?)
  , type (<)
  , Min
  , Max
  -- The |Sesh| monad
  , Sesh
  , runSesh
  , runSeshIO
  , (>>>=)
  , (>>>)
  , ireturn
  -- Session types and channels
  , Session (..)
  , Send
  , Recv
  , End
  -- Communication primitives
  , fork
  , connect
  , send
  , recv
  , close
  -- Binary choice
  , Select
  , Offer
  , selectLeft
  , selectRight
  , offerEither
  ) where

import           Prelude.Linear hiding (Max, Min, Dual)
import           Control.Concurrent.Linear
import qualified Control.Concurrent.OneShot.Linear as OneShot
import           Control.Functor.Linear
import           Data.Bifunctor.Linear
import           Data.Functor.Linear (void)
import           Data.Kind (Type)
import           Data.Type.Equality (type (==))
import           GHC.TypeLits (Nat, CmpNat, type (+))
import qualified GHC.TypeLits as Nat
import qualified GHC.TypeLits.Extra as Nat
import qualified System.IO.Linear as Linear
import           System.IO.Unsafe (unsafePerformIO)
import qualified Unsafe.Linear as Unsafe


-- * Priorities

data Priority
  = Bot
  | Val Nat
  | Top

type family (p :: Priority) <? (q :: Priority) :: Bool where
  'Bot     <? 'Bot     = 'False
  'Bot     <? q        = 'True
  ('Val n) <? ('Val m) = CmpNat n m == 'LT
  'Top     <? 'Top     = 'False
  p        <? 'Top     = 'True

type (p :: Priority) < (q :: Priority) = (p <? q) ~ 'True

type family Min (p :: Priority) (q :: Priority) :: Priority where
  Min p        'Bot     = 'Bot
  Min 'Bot     q        = 'Bot
  Min ('Val n) ('Val m) = 'Val (Nat.Min n m)
  Min p        'Top     = p
  Min 'Top     q        = q

type family Max (p :: Priority) (q :: Priority) :: Priority where
  Max p        'Bot     = p
  Max 'Bot     q        = q
  Max ('Val n) ('Val m) = 'Val (Nat.Max n m)
  Max p        'Top     = 'Top
  Max 'Top     q        = 'Bot


-- * Session types

data Send t (o :: Nat) a s = Session s => Send (OneShot.SendOnce (a, Dual s))
data Recv t (o :: Nat) a s = Session s => Recv (OneShot.RecvOnce (a, s))
data End  t (o :: Nat)     = End OneShot.SyncOnce


-- * The |Sesh| communication monad

newtype Sesh
  (t :: Type)     -- ^ Session token.
  (l :: Priority) -- ^ Lower priority bound.
  (u :: Priority) -- ^ Upper priority bound.
  (a :: Type)     -- ^ Underlying type.
  = Sesh (Linear.IO a)

-- |Unpack the |Sesh| monad.
--
-- NOTE: This operation is /unsafe/ and should not be exported.
--
unsafeRunSesh :: Sesh t l u a %1 -> Linear.IO a
unsafeRunSesh (Sesh x) = x

infixl 1 >>>=

(>>>=) :: (q < p') =>
  Sesh t p q a %1 ->
  (a %1 -> Sesh t p' q' b) %1 ->
  Sesh t (Min p p') (Max q q') b
mx >>>= mf = Sesh $ unsafeRunSesh mx >>= (unsafeRunSesh . mf)

(>>>) :: (q < p') =>
  Sesh t p q () %1 ->
  Sesh t p' q' b %1 ->
  Sesh t (Min p p') (Max q q') b
mx >>> my = mx >>>= \() -> my

ireturn :: a %1 -> Sesh t 'Top 'Bot a
ireturn x = Sesh $ return x

runSeshIO :: (forall t. Sesh t p q a) -> Linear.IO a
runSeshIO mx = unsafeRunSesh mx

runSesh :: (forall t. Sesh t p q a) -> a
runSesh mx = let (Sesh x) = mx in unsafePerformIO (Unsafe.coerce x)


-- * Duality and session initiation

class ( Consumable s      -- ^ Sessions are involutive.
      , Session (Dual s)  -- ^ The dual of a session is also a session.
      , Dual (Dual s) ~ s -- ^ Duality is involutive.
      ) => Session s where

  type Dual s = result | result -> s
  new :: Sesh t 'Top 'Bot (s, Dual s)

instance Consumable (Send t o a s) where
  consume (Send raw) = consume raw

instance Consumable (Recv t o a s) where
  consume (Recv raw) = consume raw

instance Consumable (End t o) where
  consume (End raw) = consume raw

instance Session s => Session (Send t o a s) where
  type Dual (Send t o a s) = Recv t o a (Dual s)
  new = Sesh $ bimap Send Recv <$> OneShot.new

instance Session s => Session (Recv t o a s) where
  type Dual (Recv t o a s) = Send t o a (Dual s)
  new = Sesh $ bimap Recv Send . swap <$> OneShot.new

instance Session (End t o) where
  type Dual (End t o) = End t o
  new = Sesh $ bimap End End <$> OneShot.newSync

instance Session () where
  type Dual () = ()
  new = ireturn ((), ())


-- * Communication primitives

-- |Fork off the first argument as a new  thread.
fork :: Sesh t p q () %1 -> Sesh t 'Top 'Bot ()
fork = Sesh . void . forkIO . unsafeRunSesh

-- |Combines 'new' and 'fork' in a single operation.
connect :: (Session s, 'Bot < p') =>
           (s %1 -> Sesh t p q ()) %1 ->
           (Dual s %1 -> Sesh t p' q' a) %1 ->
           Sesh t p' q' a
connect k1 k2 = new >>>= \(s1, s2) -> fork (k1 s1) >>> k2 s2

-- |Send a value over a channel.
send :: forall o s a t. Session s => (a, Send t o a s) %1 -> Sesh t ('Val o) ('Val o) s
send (x, Send ch_s) = Sesh $ do
  (here, there) <- unsafeRunSesh new
  OneShot.send ch_s (x, there)
  return here

-- |Receive a value over a channel.
recv :: forall o s a t. Session s => Recv t o a s %1 -> Sesh t ('Val o) ('Val o) (a, s)
recv (Recv ch_r) = Sesh $ OneShot.recv ch_r

-- |Close a session.
close :: forall o t. End t o %1 -> Sesh t ('Val o) ('Val o) ()
close (End sync) = Sesh $ OneShot.sync sync


-- * Binary choice

type Select t o s1 s2
  = Send t o (Either (Dual s1) (Dual s2)) ()

type Offer t o s1 s2
  = Recv t o (Either s1 s2) ()

selectLeft :: (Session s1, Session s2) =>
  Select t o s1 s2 %1 ->
  Sesh t ('Val o) ('Val o) s1
selectLeft s =
  new >>>= \(here, there) ->
  send (Left there, s) >>>
  ireturn here

selectRight :: (Session s1, Session s2) =>
  Select t o s1 s2 %1 ->
  Sesh t ('Val o) ('Val o) s2
selectRight s =
  new >>>= \(here, there) ->
  send (Right there, s) >>>
  ireturn here

offerEither :: (Session s1, Session s2, 'Bot < p, 'Val o < p) =>
  Offer t o s1 s2 %1 ->
  (Either s1 s2 %1 -> Sesh t p q a) %1 ->
  Sesh t (Min ('Val o) p) (Max ('Val o) q) a
offerEither s match =
  recv s >>>= \(x, ()) ->
  match x

-- -}
-- -}
-- -}
-- -}
-- -}
