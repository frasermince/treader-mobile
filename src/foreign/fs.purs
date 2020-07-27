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
foreign import audioDir :: String
foreign import audioBookDir :: String

foreign import _mkdir :: String -> {} -> Effect (Promise Unit)
foreign import _exists :: String -> Effect (Promise Boolean)

foreign import _readDirectory :: String -> Effect (Promise (Array File))

foreign import _writeFile :: String -> String -> String -> Effect (Promise Unit)
foreign import _readFile :: String -> String -> Effect (Promise String)
foreign import _unlink :: String -> Effect (Promise Unit)
foreign import absintheFile :: {uri :: String, name :: String, type :: String} -> String

mkdir :: String -> {} -> Aff Unit
mkdir path options = (liftEffect (_mkdir path options) >>= Promise.toAff)

unlink :: String -> Aff Unit
unlink s = (liftEffect (_unlink s) >>= Promise.toAff)
readDirectory :: String -> Aff (Array File)
readDirectory s = (liftEffect (_readDirectory s) >>= Promise.toAff)

exists :: String -> Aff Boolean
exists s = (liftEffect (_exists s) >>= Promise.toAff)

writeFile :: String -> String -> String -> Aff Unit
writeFile path d encoding = (liftEffect (_writeFile path d encoding) >>= Promise.toAff)

readFile :: String -> String -> Aff String
readFile path encoding = (liftEffect (_readFile path encoding) >>= Promise.toAff)
