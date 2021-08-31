{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Data.Type.Period
  ( Period (..),
    Empty,
    At,
    ParallelTo,
    type (<),
    type (<>),
  )
where

import Data.Kind (Constraint)
import Data.Type.Priority (Priority (..))
import Data.Type.Priority qualified as Priority

-- * Priority bounds

-- | A 'Period' describes the time between two 'Priority'
data Period = Priority :-: Priority

-- | The empty 'Period' which begins at 'Top', i.e., it never begins, and it
--   ends at 'Bot', i.e., it has already ended.
type Empty = 'Top :-: 'Bot

-- | Create a 'Period' which begins and ends at time 'o'.
type At o = 'Val o :-: 'Val o

-- | Create a 'Period' which begins at time 'o', but whose end time is unspecific.
type ParallelTo p = Begin p :-: 'Bot

type family Begin (p :: Period) :: Priority where
  Begin (begin :-: _end) = begin

type family End (p :: Period) :: Priority where
  End (_begin :-: end) = end

-- | Merges two 'Period' values, returning a 'Period' which begins at the
--   earliest beginning time and ends at the latest ending time of the two.
--
-- >>> ('Val 0 :-: 'Val 2) <> ('Val 5 :-: 'Val 8)
-- 'Val 0 :-: 'Val 8
type family (p1 :: Period) <> (p2 :: Period) :: Period where
  p1 <> p2 = (Begin p1 `Priority.Min` Begin p2) :-: (End p1 `Priority.Max` End p2)

-- | A constraint which checks if the first 'Period' ends before the second
--   'Period' begins.
type family (p1 :: Period) < (p2 :: Period) :: Constraint where
  p1 < p2 = End p1 Priority.< Begin p2
