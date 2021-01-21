{-# LANGUAGE ConstraintKinds         #-}
{-# LANGUAGE DataKinds               #-}
{-# LANGUAGE FlexibleContexts        #-}
{-# LANGUAGE FlexibleInstances       #-}
{-# LANGUAGE GADTs                   #-}
{-# LANGUAGE KindSignatures          #-}
{-# LANGUAGE MultiParamTypeClasses   #-}
{-# LANGUAGE NoImplicitPrelude       #-}
{-# LANGUAGE RankNTypes              #-}
{-# LANGUAGE ScopedTypeVariables     #-}
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
  , Chan
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
  , ChanSelect
  , ChanOffer
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

data Send (o :: Nat) (a :: Type) (s :: Type)
data Recv (o :: Nat) (a :: Type) (s :: Type)
data End  (o :: Nat)

class ( Session (Dual s)
      , Dual (Dual s) ~ s
      , Raw.Session (Raw s)
      , Raw (Dual s) ~ Raw.Dual (Raw s)
      ) => Session s where
  type Dual (s :: Type) = (result :: Type) | result -> s
  type Pr   (s :: Type) :: Nat
  type Raw  (s :: Type) :: Type

instance Session s => Session (Send o a s) where
  type Dual (Send o a s) = Recv o a (Dual s)
  type Pr   (Send o a s) = o
  type Raw  (Send o a s) = Raw.Send a (Raw s)

instance Session s => Session (Recv o a s) where
  type Dual (Recv o a s) = Send o a (Dual s)
  type Pr   (Recv o a s) = o
  type Raw  (Recv o a s) = Raw.Recv a (Raw s)

instance Session (End o) where
  type Dual (End o) = End o
  type Pr   (End o) = o
  type Raw  (End o) = Raw.End

data Chan t s = (Session s) => Chan (Raw s)


-- * Communication primitives

new :: Session s => Sesh t 'Top 'Bot (Chan t s, Chan t (Dual s))
new = Sesh $ Raw.new >>= \(here, there) -> return (Chan here, Chan there)

withNew :: Session s => ((Chan t s, Chan t (Dual s)) %1 -> Sesh t 'Top q a) %1 -> Sesh t 'Top q a
withNew mf = ibind mf new

spawn :: Sesh t p q () %1 -> Sesh t 'Top 'Bot ()
spawn mx = Sesh $ Raw.spawn (unSesh mx)

send :: Session s => (a, Chan t (Send o a s)) %1 -> Sesh t 'Top ('Val o) (Chan t s)
send (x, Chan ch) = Sesh $ Raw.send (x, ch) >>= \ch -> return (Chan ch)

recv :: Session s => Chan t (Recv o a s) %1 -> Sesh t 'Top ('Val o) (a, Chan t s)
recv (Chan ch) = Sesh $ Raw.recv ch >>= \(x, ch) -> return (x, Chan ch)

close :: Chan t (End o) %1 -> Sesh t 'Top ('Val o) ()
close (Chan ch) = Sesh $ Raw.close ch

cancel :: Session s => Chan t s %1 -> Sesh t 'Top 'Bot ()
cancel (Chan ch) = Sesh $ Raw.cancel ch


-- * Binary choice

type ChanSelect t o s1 s2
  = Chan t (Send o (Either (Chan t (Dual s1)) (Chan t (Dual s2))) (End (o + 1)))

type ChanOffer t o s1 s2
  = Chan t (Recv o (Either (Chan t s1) (Chan t s2)) (End (o + 1)))

selectLeft :: (Session s1, Session s2, Pr s1 ~ Pr s2) =>
              ChanSelect t o s1 s2 %1 ->
              Sesh t 'Top ('Val o) (Chan t s1)
selectLeft (ch :: ChanSelect t o s1 s2) =
  new >>>= \(here, there) ->
  send (Left there, ch) >>>= \ch ->
  cancel ch >>>= \() ->
  ireturn here

selectRight :: (Session s1, Session s2, Pr s1 ~ Pr s2) =>
               ChanSelect t o s1 s2 %1 ->
               Sesh t 'Top ('Val o) (Chan t s2)
selectRight ch =
  new >>>= \(here, there) ->
  send (Right there, ch) >>>= \ch ->
  cancel ch >>>= \() ->
  ireturn here

offerEither :: (Session s1, Session s2, 'Val o <= p) =>
               (Either (Chan t s1) (Chan t s2) %1 -> Sesh t p q a) %1 ->
               ChanOffer t o s1 s2 ->
               Sesh t 'Top q a
offerEither match ch =
  recv ch >>>= \(x, ch) ->
  cancel ch >>>= \() ->
  match x

-- -}
-- -}
-- -}
-- -}
-- -}
