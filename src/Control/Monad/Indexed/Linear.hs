{-# LANGUAGE NoImplicitPrelude #-}

module Control.Monad.Indexed.Linear
  ( IxFunctor(..)
  , IxPointed(..)
  , IxApplicative(..)
  , IxMonad(..)
  , IxMonadZero(..)
  , IxMonadPlus(..)
  , ijoin, (>>>=), (=<<<)
  , iapIxMonad
  ) where

import           Prelude.Linear
import qualified Control.Monad.Indexed as NonLinear
import           Data.Functor.Indexed.Linear

class (NonLinear.IxMonad m, IxApplicative m) => IxMonad m where
  ibind :: (a %1 -> m j k b) %1 -> m i j a %1 -> m i k b

ijoin :: IxMonad m => m i j (m j k a) %1 -> m i k a
ijoin = ibind id

infixr 1 =<<<
infixl 1 >>>=

(>>>=) :: IxMonad m => m i j a %1 -> (a %1 -> m j k b) %1 -> m i k b
m >>>= k = ibind k m

(=<<<) :: IxMonad m => (a %1 -> m j k b) %1 -> m i j a %1 -> m i k b
(=<<<) = ibind

iapIxMonad :: IxMonad m => m i j (a %1 -> b) %1 -> m j k a %1 -> m i k b
iapIxMonad f x = f >>>= \ f' -> x >>>= \x' -> ireturn (f' x')

class (NonLinear.IxMonadZero m, IxMonad m) => IxMonadZero m where
  imzero :: m i j a

class (NonLinear.IxMonadPlus m, IxMonadZero m) => IxMonadPlus m where
  implus :: m i j a %1 -> m i j a %1 -> m i j a

