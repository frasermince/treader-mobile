module Wiktionary where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Control.Promise as Promise
import Effect.Class (liftEffect)
import Control.Promise (Promise)

type WordReferenceResult = {query :: String, html :: String, text :: String}
foreign import _getDefinition :: String -> String -> Effect (Promise WordReferenceResult)

getDefinition :: String -> String -> Aff WordReferenceResult
getDefinition word language = (liftEffect (_getDefinition word language) >>= Promise.toAff)
