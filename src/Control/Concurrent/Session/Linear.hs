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
{-# LANGUAGE TypeApplications        #-}
{-# LANGUAGE TypeFamilies            #-}
{-# LANGUAGE TypeFamilyDependencies  #-}
{-# LANGUAGE TypeOperators           #-}
{-# LANGUAGE UndecidableInstances    #-}
{-# LANGUAGE UndecidableSuperClasses #-}


module Control.Concurrent.Session.Linear where

import           Prelude.Linear hiding (Dual, IO)
import qualified Control.Concurrent.Session.Raw.Linear as Raw
import           Control.Monad.Linear
import           Data.Kind (Type)
import           GHC.TypeLits (Nat)
import qualified GHC.TypeLits as Nat
import qualified System.IO.Linear as Linear
import           System.IO.Unsafe (unsafePerformIO)
import qualified Unsafe.Linear as Unsafe

data Priority
  = Bot
  | Val Nat
  | Top

type family (p :: Priority) <=? (q :: Priority) :: Bool where
  'Bot   <=? 'Bot   = 'True
  'Bot   <=? 'Val m = 'True
  'Bot   <=? 'Top   = 'True
  'Val n <=? 'Bot   = 'False
  'Val n <=? 'Val m = n Nat.<=? m
  'Val n <=? 'Top   = 'True
  'Top   <=? 'Bot   = 'False
  'Top   <=? 'Val m = 'False
  'Top   <=? 'Top   = 'True

type (p :: Priority) <= (q :: Priority) = (p <=? q) ~ 'True

newtype Sesh
  (t :: Type)     -- ^ Session token.
  (l :: Priority) -- ^ Lower priority bound.
  (u :: Priority) -- ^ Upper priority bound.
  (a :: Type)     -- ^ Underlying type.
  = Sesh (Linear.IO a)

-- |Unsafely unwrap a session.
unSesh :: Sesh t l u a %1 -> Linear.IO a
unSesh (Sesh x) = x

data Send (o :: Nat) (a :: Type) (s :: Type)
data Recv (o :: Nat) (a :: Type) (s :: Type)
data End  (o :: Nat)

class ( Session (Dual s)
      , Dual (Dual s) ~ s
      , Raw.Session (Repr s)
      , Repr (Dual s) ~ Raw.Dual (Repr s)
      ) => Session s where
  type Dual s = result | result -> s
  type Repr s

instance Session s => Session (Send o a s) where
  type Dual (Send o a s) = Recv o a (Dual s)
  type Repr (Send o a s) = Raw.Send a (Repr s)

instance Session s => Session (Recv o a s) where
  type Dual (Recv o a s) = Send o a (Dual s)
  type Repr (Recv o a s) = Raw.Recv a (Repr s)

instance Session (End o) where
  type Dual (End o) = End o
  type Repr (End o) = Raw.End

data Chan t s = (Session s) => Chan (Repr s)

new :: Session s => Sesh t 'Top 'Bot (Chan t s, Chan t (Dual s))
new = Sesh $ Raw.new >>= \(here, there) -> return (Chan here, Chan there)

ibind :: (q <= p') => (a %1 -> Sesh t p' q' b) %1 -> Sesh t p q a %1 -> Sesh t p q' b
ibind mf mx = Sesh $ unSesh mx >>= (unSesh . mf)

ireturn :: a -> Sesh t p p a
ireturn x = Sesh $ return x

withNew :: Session s => ((Chan t s, Chan t (Dual s)) %1 -> Sesh t 'Top q a) -> Sesh t 'Top q a
withNew mf = ibind mf new

spawn :: Sesh t p q () %1 -> Sesh t 'Top 'Bot ()
spawn mx = Sesh $ Raw.spawn (unSesh mx)

send :: Session s => (a, Chan t (Send o a s)) %1 -> Sesh t 'Top ('Val o) (Chan t s)
send (x, Chan ch) = Sesh $ Raw.send (x, ch) >>= \ch -> return (Chan ch)

recv :: Session s => Chan t (Recv o a s) %1 -> Sesh t 'Top ('Val o) (a, Chan t s)
recv (Chan ch) = Sesh $ Raw.recv ch >>= \(x, ch) -> return (x, Chan ch)

close :: Chan t (End o) %1 -> Sesh t 'Top ('Val o) ()
close (Chan ch) = Sesh $ Raw.close ch

runSesh :: (forall t. Sesh t p q a) -> Linear.IO a
runSesh mx = let (Sesh x) = mx in unsafePerformIO (Unsafe.coerce x)

-- -}
-- -}
-- -}
-- -}
-- -}
