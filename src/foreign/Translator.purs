module Translator where

import Prelude
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Effect (Effect)
import Data.Nullable (toMaybe, Nullable)
import Data.Maybe (Maybe(..))

type TranslationResponse = String

foreign import _translate :: String -> String -> String -> Effect (Promise TranslationResponse)

translate :: String -> String -> String -> Aff TranslationResponse
translate apiKey sourceLang snippet = (liftEffect (_translate apiKey sourceLang snippet) >>= Promise.toAff)

