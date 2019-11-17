module AsyncStorage where


import Prelude
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect (Effect)

foreign import _setItem :: String -> String -> Effect (Promise Unit)
foreign import _clear :: Effect (Promise Unit)

setItem :: String -> String -> Aff Unit
setItem x y = (liftEffect (_setItem x y) >>= Promise.toAff)

clear = liftEffect _clear >>= Promise.toAff
