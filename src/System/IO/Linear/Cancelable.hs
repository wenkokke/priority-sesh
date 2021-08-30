{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE StandaloneDeriving #-}

module System.IO.Linear.Cancelable where

import Prelude.Linear
import Control.Functor.Linear   qualified as Linear (Monad(..), return)
import Data.Functor.Linear      qualified as Linear (Traversable(..))
import Data.List.NonEmpty       (NonEmpty(..))
import Data.Unrestricted.Linear (Consumable(..))
import System.IO.Linear         qualified as Linear (IO, fromSystemIO)
import System.Mem.Weak          (Weak)
import System.Mem.Weak          qualified as Weak (finalize)
import Unsafe.Linear            qualified as Unsafe (toLinear)
import qualified Data.Foldable as Linear
import Data.Bitraversable (Bitraversable)

-- |'Cancelable' values are linear values which may be discarded,
--  but which require cleanup in the form of an 'IO' action.
class Cancelable a where
  cancel :: a %1 -> Linear.IO ()


-- * Cancelable references

instance Cancelable (Weak a) where
  cancel :: Weak a %1 -> Linear.IO ()
  cancel weak = Linear.fromSystemIO (Unsafe.toLinear Weak.finalize weak)


-- * Deriving 'Cancelable'


-- ** Deriving 'Cancelable' via 'Consumable'

newtype FromConsumable a = FromConsumable a

instance Consumable a => Cancelable (FromConsumable a) where
  cancel (FromConsumable a) = Linear.return (consume a)

deriving via (FromConsumable Bool)     instance Cancelable Bool
deriving via (FromConsumable Char)     instance Cancelable Char
deriving via (FromConsumable Double)   instance Cancelable Double
deriving via (FromConsumable Int)      instance Cancelable Int
deriving via (FromConsumable Ordering) instance Cancelable Ordering
deriving via (FromConsumable ())       instance Cancelable ()
deriving via (FromConsumable Any)      instance Cancelable Any
deriving via (FromConsumable All)      instance Cancelable All


-- ** Deriving 'Cancellable' via 'Traversable'

newtype FromTraversable t a = FromTraversable (t a)

instance (Linear.Traversable t, Consumable (t ()), Cancelable a) => Cancelable (FromTraversable t a) where
  cancel (FromTraversable ta) = do
    let (>>=) = (Linear.>>=) in do
      tu <- Linear.traverse cancel ta
      Linear.return (consume tu)

deriving via (FromTraversable [] a)    instance Cancelable a => Cancelable [a]
deriving via (FromTraversable Maybe a) instance Cancelable a => Cancelable (Maybe a)


-- ** Deriving 'Cancelable' via 'Bitraversable'

-- NOTE: There currently is no linear 'Bitraversable', so the two most important
--       instances are simply derived by hand.

-- TODO: If 'Bitraversable' is added to linear-base, this should be replaced by
--       an implementation using 'DerivingVia'.

instance (Cancelable a, Cancelable b) => Cancelable (a, b) where
  cancel (a, b) =
    let (>>) = (Linear.>>) in do
      cancel a
      cancel b

instance (Cancelable a, Cancelable b) => Cancelable (Either a b) where
  cancel (Left  a) = cancel a
  cancel (Right b) = cancel b
