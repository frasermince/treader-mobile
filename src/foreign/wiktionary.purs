module Wiktionary where

import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Control.Promise as Promise
import Effect.Class (liftEffect)
import Control.Promise (Promise)

type ParsedExample = {example :: String, translation :: String}
type Definition = Array {definition :: String, parsedExamples :: Array ParsedExample, examples :: Array String}
type WiktionaryResult = Array {partOfSpeech :: String, language :: String, definitions :: Definition}
foreign import _getDefinition :: String -> String -> String -> Effect (Promise WiktionaryResult)

getDefinition :: String -> String -> String -> Aff WiktionaryResult
getDefinition word locale language = (liftEffect (_getDefinition word locale language) >>= Promise.toAff)
