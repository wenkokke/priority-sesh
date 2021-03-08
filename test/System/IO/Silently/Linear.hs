{-# LANGUAGE LinearTypes #-}

module System.IO.Silently.Linear
  ( silence
  , capture
  , capture_
  ) where

import qualified Prelude
import           Prelude.Linear
import           System.IO.Linear (fromSystemIO)
import qualified System.IO.Linear as Linear (IO)
import qualified System.IO.Silently as Silently
import qualified Unsafe.Linear as Unsafe


silence :: Linear.IO a %1 -> Linear.IO a
silence x = fromSystemIO (Unsafe.toLinear Silently.silence (toSystemIO x))

capture :: Linear.IO a %1 -> Linear.IO (String, a)
capture x = fromSystemIO (Unsafe.toLinear Silently.capture (toSystemIO x))

capture_ :: Linear.IO a %1 -> Linear.IO String
capture_ x = fromSystemIO (Unsafe.toLinear Silently.capture_ (toSystemIO x))

toSystemIO :: Linear.IO a %1-> IO a
toSystemIO = Unsafe.coerce
