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
  , Session (Dual)
  , Send
  , Recv
  , End
  -- Communication primitives
  , new
  , fork
  , send
  , recv
  , close
  , cancel
  -- Binary choice
  , Select
  , Offer
  , selectLeft
  , selectRight
  , offerEither
  ) where

import           Prelude.Linear hiding (Max, Min, Dual)
import           Control.Concurrent.Linear
import qualified Control.Concurrent.Session.Raw.Linear as Raw
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

data Send t (o :: Nat) a s = Session s => Send (Raw.Send a (Raw s))
data Recv t (o :: Nat) a s = Session s => Recv (Raw.Recv a (Raw s))
data End  t (o :: Nat)     = End Raw.End


-- * Duality and conversion to Raw representation

class ( Consumable s                     -- Sessions are involutive.
      , Session (Dual s)                 -- The dual of a session is also a session.
      , Dual (Dual s) ~ s                -- Duality is involutive.
      , Raw.Session (Raw s)              -- The Raw representation is also a session.
      , Raw (Dual s) ~ Raw.Dual (Raw s)  -- Duality and Raw commute.
      ) => Session s where

  type Dual s = result | result -> s
  type Raw  s

  toRaw   :: s %1 -> Raw s
  fromRaw :: Raw s %1 -> s

instance Consumable (Send t o a s) where
  consume (Send raw) = consume raw

instance Consumable (Recv t o a s) where
  consume (Recv raw) = consume raw

instance Consumable (End t o) where
  consume (End raw) = consume raw

instance Session s => Session (Send t o a s) where
  type Dual (Send t o a s) = Recv t o a (Dual s)
  type Raw  (Send t o a s) = Raw.Send a (Raw s)

  toRaw (Send s) = s
  fromRaw s = Send s

instance Session s => Session (Recv t o a s) where
  type Dual (Recv t o a s) = Send t o a (Dual s)
  type Raw  (Recv t o a s) = Raw.Recv a (Raw s)

  toRaw (Recv s) = s
  fromRaw s = Recv s

instance Session (End t o) where
  type Dual (End t o) = End t o
  type Raw  (End t o) = Raw.End

  toRaw (End s) = s
  fromRaw s = End s

instance Session () where
  type Dual () = ()
  type Raw  () = ()

  toRaw () = ()
  fromRaw () = ()


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


-- * Communication primitives

-- |Create a new channel with two dual endpoints.
new :: Session s => Sesh t 'Top 'Bot (s, Dual s)
new = Sesh $ bimap fromRaw fromRaw <$> Raw.new

-- |Fork off the first argument as a new  thread.
fork :: Sesh t p q () %1 -> Sesh t 'Top 'Bot ()
fork = Sesh . void . forkIO . unsafeRunSesh

-- |Send a value over a channel.
send :: forall o s a t. Session s => (a, Send t o a s) %1 -> Sesh t ('Val o) ('Val o) s
send (x, s) = Sesh $ do
  s <- Raw.send (x, toRaw s)
  return (fromRaw s)

-- |Receive a value over a channel.
recv :: forall o s a t. Session s => Recv t o a s %1 -> Sesh t ('Val o) ('Val o) (a, s)
recv s = Sesh $ do
  (x, s) <- Raw.recv (toRaw s)
  return (x, fromRaw s)

-- |Close a session.
close :: forall o t. End t o %1 -> Sesh t ('Val o) ('Val o) ()
close s = Sesh $ Raw.close (toRaw s)

-- |Cancel a session.
cancel :: forall s t. Session s => s %1 -> Sesh t 'Top 'Bot ()
cancel s = Sesh $ Raw.cancel (toRaw s)


-- * Binary choice

type Select t o s1 s2
  = Send t o (Either (Dual s1) (Dual s2)) (End t (o + 1))

type Offer t o s1 s2
  = Recv t o (Either s1 s2) (End t (o + 1))

selectLeft :: (Session s1, Session s2) =>
  Select t o s1 s2 %1 ->
  Sesh t ('Val o) ('Val o) s1
selectLeft s =
  new >>>= \(here, there) ->
  send (Left there, s) >>>= \s ->
  cancel s >>>= \() ->
  ireturn here

selectRight :: (Session s1, Session s2) =>
  Select t o s1 s2 %1 ->
  Sesh t ('Val o) ('Val o) s2
selectRight s =
  new >>>= \(here, there) ->
  send (Right there, s) >>>= \s ->
  cancel s >>>= \() ->
  ireturn here

offerEither :: (Session s1, Session s2, 'Bot < p, 'Val o < p) =>
  (Either s1 s2 %1 -> Sesh t p q a) %1 ->
  Offer t o s1 s2 %1 ->
  Sesh t (Min ('Val o) p) (Max ('Val o) q) a
offerEither match s =
  recv s >>>= \(x, s) ->
  cancel s >>>= \() ->
  match x

-- -}
-- -}
-- -}
-- -}
-- -}
