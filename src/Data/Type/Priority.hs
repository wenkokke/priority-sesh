{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Priority
  ( Priority (..)
  , type Min
  , type Max
  , type (+)
  , type (<)
  ) where

import           Data.Kind (Constraint)
import           Data.Type.Bool (If)
import           Data.Type.Nat (Nat(..), N0, N1, N2, N3, N4, N5, N6, N7, N8, N9)
import qualified Data.Type.Nat as Nat

data Priority
  = Bot
  | Val Nat
  | Top

-- | Type-level '<' as a constraint
type family (p :: Priority) < (q :: Priority) :: Constraint where
  'Bot   < 'Val n = ()
  'Val m < 'Val n = m Nat.< n
  'Bot   < 'Top   = ()
  'Val m < 'Top   = ()

-- | Type-level 'max'
type family Min (p :: Priority) (q :: Priority) :: Priority where
  Min p        'Bot     = 'Bot
  Min 'Bot     q        = 'Bot
  Min ('Val n) ('Val m) = 'Val (Nat.Min n m)
  Min p        'Top     = p
  Min 'Top     q        = q

-- | Type-level 'min'
type family Max (p :: Priority) (q :: Priority) :: Priority where
  Max p        'Bot     = p
  Max 'Bot     q        = q
  Max ('Val n) ('Val m) = 'Val (Nat.Max n m)
  Max p        'Top     = 'Top
  Max 'Top     q        = 'Bot

-- | Type-level '+'
type family (p :: Priority) + (q :: Priority) :: Priority where
  'Bot   + q      = 'Bot
  p      + 'Bot   = 'Bot
  'Val m + 'Val n = 'Val (m Nat.+ n)
  'Top   + q      = 'Top
  p      + 'Top   = 'Top
