module FS where

import Prelude
import Effect.Aff (Aff)
import Effect (Effect)
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)

type File
  = { path :: String, name :: String }

foreign import bookDir :: String

foreign import _exists :: String -> Effect (Promise Boolean)

foreign import _readDirectory :: String -> Effect (Promise (Array File))

readDirectory :: String -> Aff (Array File)
readDirectory s = (liftEffect (_readDirectory s) >>= Promise.toAff)

exists :: String -> Aff Boolean
exists s = (liftEffect (_exists s) >>= Promise.toAff)
