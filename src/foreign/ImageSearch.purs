module ImageSearch where
import Prelude
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect (Effect)
import Data.Nullable (toMaybe, Nullable)

import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..), try)
foreign import _imageSearch :: String -> Int -> Int -> Effect (Promise (Array Image))
type Image = {link :: String}

imageSearch :: String -> Int -> Int -> Aff (Array Image)
imageSearch keyword low high = (liftEffect (_imageSearch keyword low high) >>= Promise.toAff)

