module ImageSearch where
import Prelude
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect (Effect)
import Data.Nullable (toMaybe, Nullable, toNullable)
import Data.Maybe (Maybe(..))

import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..), try)
foreign import _imageSearch :: String -> Int -> Int -> Nullable String -> Effect (Promise (Array Image))
type Image = {link :: String, image :: {thumbnailLink :: String}}

imageSearch :: String -> Int -> Int -> Maybe String -> Aff (Array Image)
imageSearch keyword low high language = (liftEffect (_imageSearch keyword low high $ toNullable language) >>= Promise.toAff)

