{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Priority
  ( Priority (..),
    type (<),
    type Min,
    type Max,
  )
where

import Data.Kind (Constraint)
import Data.Type.Bool (If)
import Data.Type.Nat.Extra (Nat (..))
import Data.Type.Nat.Extra qualified as Nat

-- * Priorities

-- | A 'Priority' is an abstract representation of the time at which some event
--   happens.
data Priority
  = -- | The event happens before every other event.
    Bot
  | -- | A concrete time, represented by a natural number.
    Val Nat
  | -- | The event happens after every other event.
    Top

-- | Type-level '<' as a constraint
type family (p :: Priority) < (q :: Priority) :: Constraint where
  'Bot < 'Val n = ()
  'Val m < 'Val n = m Nat.< n
  'Bot < 'Top = ()
  'Val m < 'Top = ()

-- | Type-level 'max'
type family Min (p :: Priority) (q :: Priority) :: Priority where
  Min p 'Bot = 'Bot
  Min 'Bot q = 'Bot
  Min ('Val n) ('Val m) = 'Val (Nat.Min n m)
  Min p 'Top = p
  Min 'Top q = q

-- | Type-level 'min'
type family Max (p :: Priority) (q :: Priority) :: Priority where
  Max p 'Bot = p
  Max 'Bot q = q
  Max ('Val n) ('Val m) = 'Val (Nat.Max n m)
  Max p 'Top = 'Top
  Max 'Top q = 'Bot
