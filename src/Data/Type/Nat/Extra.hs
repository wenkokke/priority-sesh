{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE NoStarIsType #-}

module Data.Type.Nat.Extra
  ( Nat (..),
    type Max,
    type Min,
    type (+),
    type (*),
    type (<),
  )
where

import Data.Type.Bool (If)
import Data.Type.Equality (type (==))
import GHC.TypeNats (CmpNat, Nat (..), type (*), type (+), type (<=?))
import Prelude.Linear (Ordering (LT))

infix 4 <

infixl 9 `Min`, `Max`

-- | Type-level '<' as a constraint
type (m :: Nat) < (n :: Nat) = CmpNat m n ~ 'LT

-- | Type-level 'max'
type Max (m :: Nat) (n :: Nat) = If (CmpNat m n == 'LT) n m

-- | Type-level 'min'
type Min (m :: Nat) (n :: Nat) = If (CmpNat m n == 'LT) m n
