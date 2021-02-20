{-# LANGUAGE NoImplicitPrelude #-}

module Data.Functor.Indexed.Linear
  ( IxFunctor(..)
  , IxCopointed(..)
  , IxPointed(..)
  , IxApplicative(..)
  , (<<$>>), (<<*>>), (<<*), (*>>)
  ) where

import           Prelude.Linear
import qualified Data.Functor.Indexed as Data
import           Data.Unrestricted.Linear

class Data.IxFunctor f => IxFunctor f where
  imap :: (a %1 -> b) %1 -> f j k a %1 -> f j k b

-- | Infix alias of 'imap'.  Or, ('<$>') for 'IxFunctor'.  Should be
-- interchangeable with ('<$>'), but requires 'IxFunctor' constraints instead
-- of (possibly many) 'Functor' constraints.
infixl 4 <<$>>
(<<$>>) :: IxFunctor f => (a %1 -> b) %1 -> f j k a %1 -> f j k b
(<<$>>) = imap

class (Data.IxApplicative m, IxPointed m) => IxApplicative m where
  iap :: m i j (a %1 -> b) %1 -> m j k a %1 -> m i k b

-- | Infix alias of 'iap'.  Or, ('<*>') for 'IxApplicative'.
infixl 4 <<*>>
(<<*>>) :: IxApplicative f => f i j (a %1 -> b) %1 -> f j k a %1 -> f i k b
(<<*>>) = iap

-- | ('Control.Applicative.<*') for 'IxApplicative'.
infixl 4 <<*
(<<*) :: (IxApplicative f, Consumable b) => f i j a %1 -> f j k b %1 -> f i k a
(<<*) a b = imap (flip lseq) a <<*>> b

-- | ('Control.Applicative.*>') for 'IxApplicative'.
infixl 4 *>>
(*>>) :: (IxApplicative f, Consumable a) => f i j a %1 -> f j k b %1 -> f i k b
(*>>) a b = imap lseq a <<*>> b

class (Data.IxPointed m, IxFunctor m) => IxPointed m where
  ireturn :: a %1 -> m i i a

class (Data.IxCopointed w, IxFunctor w) => IxCopointed w where
  iextract :: w i i a %1 -> a

