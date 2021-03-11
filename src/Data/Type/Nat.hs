{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Nat
  ( Nat(..)
  , N0, N1, N2, N3, N4, N5, N6, N7, N8, N9
  , type Max
  , type Min
  , type (+)
  , type (<)
  ) where

{- using GHC.TypeNats -}

-- {-

import Data.Type.Equality (type (==))
import Data.Type.Bool (If)
import GHC.TypeNats (Nat(..), CmpNat, type (<=?), type (+))

type S n = 1 + n

type N0 = 0
type N1 = 1
type N2 = 2
type N3 = 3
type N4 = 4
type N5 = 5
type N6 = 6
type N7 = 7
type N8 = 8
type N9 = 9

-- | Type-level '<' as a constraint
type (m :: Nat) < (n :: Nat) = CmpNat m n ~ 'LT

-- | Type-level 'max'
type Max (m :: Nat) (n :: Nat) = If (m <=? n) n m

-- | Type-level 'min'
type Min (m :: Nat) (n :: Nat) = If (m <=? n) m n

-- -}

{- using hand-rolled naturals -}

{-

import Data.Kind (Constraint)
import Data.Type.Bool (If)

data Nat
  = Z
  | S Nat

type N0 = 'Z
type N1 = 'S N0
type N2 = 'S N1
type N3 = 'S N2
type N4 = 'S N3
type N5 = 'S N4
type N6 = 'S N5
type N7 = 'S N6
type N8 = 'S N7
type N9 = 'S N8

-- | Type-level '+'
type family (m :: Nat) + (n :: Nat) :: Nat where
  'Z   + n = n
  'S m + n = 'S (m + n)

-- | Check if 'm' is equal to 'c + n' for some c
type family (m :: Nat) `IsConstPlus` (n :: Nat) :: Bool where
  n      `IsConstPlus` n = 'True
  ('S m) `IsConstPlus` n = m `IsConstPlus` n

-- | Type-level 'max'
type family Max (m :: Nat) (n :: Nat) :: Nat where
  Max m      m      = m
  Max 'Z     n      = n
  Max m      'Z     = m
  Max ('S m) ('S n) = 'S (Max m n)

-- | Type-level 'min'
type family Min (m :: Nat) (n :: Nat) :: Nat where
  Min m      m      = m
  Min 'Z     n      = 'Z
  Min m      'Z     = 'Z
  Min ('S m) ('S n) = 'S (Min m n)

-- | Type-level '<='
type family (m :: Nat) <= (n :: Nat) :: Constraint where
  m    <= m    = ()
  'Z   <= n    = ()
  'S m <= 'S n = m <= n

-- | Type-level '<' as a constraint
type (m :: Nat) < (n :: Nat) = S m <= n

-- -}
