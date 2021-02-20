{-# LANGUAGE DataKinds            #-}
{-# LANGUAGE TypeFamilies         #-}
{-# LANGUAGE TypeOperators        #-}
{-# LANGUAGE UndecidableInstances #-}

-- NOTE: taken from `ghc-typelits-extra`.
--
module GHC.TypeLits.Extra where

import Data.Type.Bool (If)
import GHC.TypeLits (Nat, type (<=?))

-- | Type-level 'max'
type family Max (x :: Nat) (y :: Nat) :: Nat where
  Max n n = n
  Max x y = If (x <=? y) y x

-- | Type-level 'min'
type family Min (x :: Nat) (y :: Nat) :: Nat where
  Min n n = n
  Min x y = If (x <=? y) x y
