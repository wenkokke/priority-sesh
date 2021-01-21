{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
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
  , type (<=?)
  , type (<=)
  -- The |Sesh| monad
  , Sesh
  , ibind
  , (=<<<)
  , (>>>=)
  , ireturn
  -- Session types and channels
  , Session (Dual, Pr)
  , Send
  , Recv
  , End
  -- Communication primitives
  , withNew
  , spawn
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

import           Prelude.Linear hiding (Dual, IO)
import qualified Control.Concurrent.Session.Raw.Linear as Raw
import           Control.Monad.Linear
import           Data.Kind (Type)
import           GHC.TypeLits (Nat, type (+))
import qualified GHC.TypeLits as Nat
import qualified System.IO.Linear as Linear
import           System.IO.Unsafe (unsafePerformIO)
import qualified Unsafe.Linear as Unsafe


-- * Priorities

data Priority
  = Bot
  | Val Nat
  | Top

type family (p :: Priority) <=? (q :: Priority) :: Bool where
  'Bot   <=? p      = 'True
  'Val n <=? 'Bot   = 'False
  'Val n <=? 'Val m = n Nat.<=? m
  'Val n <=? 'Top   = 'True
  'Top   <=? 'Bot   = 'False
  'Top   <=? 'Val m = 'False
  'Top   <=? 'Top   = 'True

type (p :: Priority) <= (q :: Priority) = (p <=? q) ~ 'True


-- * The |Sesh| monad

newtype Sesh
  (t :: Type)     -- ^ Session token.
  (l :: Priority) -- ^ Lower priority bound.
  (u :: Priority) -- ^ Upper priority bound.
  (a :: Type)     -- ^ Underlying type.
  = Sesh (Linear.IO a)

unSesh :: Sesh t l u a %1 -> Linear.IO a
unSesh (Sesh x) = x

ibind :: (q <= p') => (a %1 -> Sesh t p' q' b) %1 -> Sesh t p q a %1 -> Sesh t p q' b
ibind mf mx = Sesh $ unSesh mx >>= (unSesh . mf)

infixr 1 =<<<
infixl 1 >>>=

(=<<<) :: (q <= p') => (a %1 -> Sesh t p' q' b) %1 -> Sesh t p q a %1 -> Sesh t p q' b
(=<<<) mf mx = Sesh $ unSesh mx >>= (unSesh . mf)

(>>>=) :: (q <= p') => Sesh t p q a %1 -> (a %1 -> Sesh t p' q' b) %1 -> Sesh t p q' b
(>>>=) = flip ibind

ireturn :: a %1 -> Sesh t p p a
ireturn x = Sesh $ return x

runSesh :: (forall t. Sesh t p q a) -> Linear.IO a
runSesh mx = let (Sesh x) = mx in unsafePerformIO (Unsafe.coerce x)


-- * Session types and channels

data Send (t :: Type) (o :: Nat) (a :: Type) (s :: Type) where
  Send :: Session s => Raw.Send a (Raw s) %1 -> Send t o a s

data Recv (t :: Type) (o :: Nat) (a :: Type) (s :: Type) where
  Recv :: Session s => Raw.Recv a (Raw s) %1 -> Recv t o a s

data End  (t :: Type) (o :: Nat) where
  End :: Raw.End %1 -> End t o

class ( Session (Dual s)
      , Dual (Dual s) ~ s
      , Raw.Session (Raw s)
      , Raw (Dual s) ~ Raw.Dual (Raw s)
      ) => Session s where
  type Dual (s :: Type) = (result :: Type) | result -> s
  type Pr   (s :: Type) :: Nat
  type Raw  (s :: Type) :: Type

  toRaw   :: s %1 -> Raw s
  fromRaw :: Raw s %1 -> s

instance Session s => Session (Send t o a s) where
  type Dual (Send t o a s) = Recv t o a (Dual s)
  type Pr   (Send t o a s) = o
  type Raw  (Send t o a s) = Raw.Send a (Raw s)

  toRaw (Send s) = s
  fromRaw s = Send s

instance Session s => Session (Recv t o a s) where
  type Dual (Recv t o a s) = Send t o a (Dual s)
  type Pr   (Recv t o a s) = o
  type Raw  (Recv t o a s) = Raw.Recv a (Raw s)

  toRaw (Recv s) = s
  fromRaw s = Recv s

instance Session (End t o) where
  type Dual (End t o) = End t o
  type Pr   (End t o) = o
  type Raw  (End t o) = Raw.End

  toRaw (End s) = s
  fromRaw s = End s


-- * Communication primitives

new :: Session s => Sesh t 'Top 'Bot (s, Dual s)
new = Sesh $ do
  (here, there) <- Raw.new
  return (fromRaw here, fromRaw there)

withNew :: Session s => ((s, Dual s) %1 -> Sesh t 'Top q a) %1 -> Sesh t 'Top q a
withNew mf =
  ibind mf new

spawn :: Sesh t p q () %1 -> Sesh t 'Top 'Bot ()
spawn mx = Sesh $
  Raw.spawn (unSesh mx)

send :: Session s => (a, Send t o a s) %1 -> Sesh t 'Top ('Val o) (s)
send (x, s) = Sesh $ do
  s <- Raw.send (x, toRaw s)
  return (fromRaw s)

recv :: Session s => Recv t o a s %1 -> Sesh t 'Top ('Val o) (a, s)
recv s = Sesh $ do
  (x, s) <- Raw.recv (toRaw s)
  return (x, fromRaw s)

close :: End t o %1 -> Sesh t 'Top ('Val o) ()
close s = Sesh $
  Raw.close (toRaw s)

cancel :: Session s => s %1 -> Sesh t 'Top 'Bot ()
cancel s = Sesh $
  Raw.cancel (toRaw s)


-- * Binary choice

type Select t o s1 s2 = Send t o (Either (Dual s1) (Dual s2)) (End t (o + 1))

type Offer t o s1 s2 = Recv t o (Either s1 s2) (End t (o + 1))

selectLeft :: (Session s1, Session s2, Pr s1 ~ Pr s2) =>
              Select t o s1 s2 %1 ->
              Sesh t 'Top ('Val o) s1
selectLeft s =
  new >>>= \(here, there) ->
  send (Left there, s) >>>= \s ->
  cancel s >>>= \() ->
  ireturn here

selectRight :: (Session s1, Session s2, Pr s1 ~ Pr s2) =>
               Select t o s1 s2 %1 ->
               Sesh t 'Top ('Val o) s2
selectRight s =
  new >>>= \(here, there) ->
  send (Right there, s) >>>= \s ->
  cancel s >>>= \() ->
  ireturn here

offerEither :: (Session s1, Session s2, 'Val o <= p) =>
               (Either s1 s2 %1 -> Sesh t p q a) %1 ->
               Offer t o s1 s2 ->
               Sesh t 'Top q a
offerEither mats s =
  recv s >>>= \(x, s) ->
  cancel s >>>= \() ->
  mats x

-- -}
-- -}
-- -}
-- -}
-- -}
