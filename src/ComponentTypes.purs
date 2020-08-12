module ComponentTypes where
import Prelude
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Effect (Effect)

type NullableContext = {sentence :: Nullable String, phrase :: Nullable String, surrounding :: Nullable String, sentenceOffset :: Nullable Int, phraseOffset :: Nullable Int, wordLength :: Int}

type Context = {sentence :: Maybe String, phrase :: Maybe String, surrounding :: Maybe String, sentenceOffset :: Maybe Int, phraseOffset :: Maybe Int, wordLength :: Int}
type Translation = {text :: String, isPermitted :: Boolean}

type Selection = {id :: Int, word :: String, sentence :: String, phrase :: String, sentenceOffset :: Int, phraseOffset :: Int, wordLength :: Int, book :: {language :: String, id :: Int}, wordTranslation :: String, sentenceTranslation :: String, phraseTranslation :: String}

type FlashcardOffset = {word :: String, offset :: Int}
type FlashcardOffsetTranslation = {word :: String, offset :: Int, translation :: String}
type FlashcardExistence = {with :: Array FlashcardOffset, without :: Array FlashcardOffsetTranslation}
type Sentence
  = {text :: String, translation :: String, flashcardExistence :: FlashcardExistence, audioUrl :: String}

type Flashcard = {id :: String, word :: String, sentence :: {text :: String, translation :: String, audioUrl :: String, id :: String}, imageUrl :: Array String, a :: Number, b :: Number, t :: Number, startOffset :: Int, hoursPassed :: Number}

type BookViewQuery
  = { book :: { epubUrl :: Nullable String, processedEpubUrl :: Nullable String, id :: String, audioChapters :: Array {audioUrl :: String, chapter :: Int} } }
type AudioInformation = {startPageTime :: String, endPageTime :: String, index :: Int}

type StateChange a = (a -> a) -> Effect Unit
