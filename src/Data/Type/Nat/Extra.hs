{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE NoStarIsType    #-}
{-# LANGUAGE TypeFamilies    #-}
{-# LANGUAGE TypeOperators   #-}

module Data.Type.Nat.Extra
  ( Nat(..)
  , type Max
  , type Min
  , type (+)
  , type (*)
  , type (<)
  ) where

import Prelude.Linear     ( Ordering(LT) )
import Data.Type.Equality ( type (==) )
import Data.Type.Bool     ( If )
import GHC.TypeNats       ( Nat(..), CmpNat, type (<=?), type (+), type (*) )

infix  4 <
infixl 9 `Min`, `Max`

-- | Type-level '<' as a constraint
type (m :: Nat) < (n :: Nat) = CmpNat m n ~ 'LT

-- | Type-level 'max'
type Max (m :: Nat) (n :: Nat) = If (CmpNat m n == 'LT) n m

-- | Type-level 'min'
type Min (m :: Nat) (n :: Nat) = If (CmpNat m n == 'LT) m n
