module ComponentTypes where
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Nullable (Nullable, toMaybe, toNullable, null)

type NullableContext = {sentence :: Nullable String, phrase :: Nullable String, surrounding :: Nullable String, sentenceOffset :: Nullable Int, phraseOffset :: Nullable Int, wordLength :: Int}
type Context = {sentence :: Maybe String, phrase :: Maybe String, surrounding :: Maybe String, sentenceOffset :: Maybe Int, phraseOffset :: Maybe Int, wordLength :: Int}
type Translation = {text :: String, isPermitted :: Boolean}

type Selection = {word :: String, sentence :: String, phrase :: String, sentenceOffset :: Int, phraseOffset :: Int, wordLength :: Int, book :: {language :: String, id :: Int}}
