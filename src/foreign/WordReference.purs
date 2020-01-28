module WordReference where

import Prelude
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect (Effect)

type Translation = {from :: String, fromType :: String, to :: String, toType :: String}
type TranslationResult = {title :: String, translations :: Array Translation}
type WordReferenceResult = {pronWR :: String, audio :: Array String, translations :: Array TranslationResult}


foreign import _wordReference :: String -> String -> String -> Effect (Promise WordReferenceResult)

wordReference :: String -> String -> String -> Aff WordReferenceResult
wordReference a b c = (liftEffect (_wordReference a b c) >>= Promise.toAff)
