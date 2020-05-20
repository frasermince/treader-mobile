module FirebaseMessaging where

import Prelude
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect (Effect)
import Data.Nullable (toMaybe, Nullable)
import Data.Maybe (Maybe(..))

foreign import _requestPermission :: Effect (Promise Unit)

requestPermission :: Aff Unit
requestPermission = (liftEffect _requestPermission >>= Promise.toAff)
