module FlashcardBuilder.SentenceChoice where

import Prelude
import Paper (textInput, surface, button, title, divider, listItem, listIcon, caption)
import React.Basic.Hooks as React
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import Record.Unsafe.Union (unsafeUnion)
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Effect.Aff (Aff, launchAff_, try)
import React.Basic.Events (EventFn, unsafeEventFn)
import ApolloHooks (useMutation, gql)
import Unsafe.Coerce (unsafeCoerce)
import Effect.Class (liftEffect)
import Data.Undefinable (toUndefinable)
import Data.Maybe (Maybe(..))
import Markup as M
import QueryHooks (useData, UseData)
import Type.Proxy (Proxy(..))
import Effect.Exception (message)
import Image (_image)
import Data.String (length)
import React.Basic.Native as RN
import Paper (title)
import Config (config)
import Translator (translate)
import Record.Unsafe (unsafeGet)
import FlashcardBuilder.Util(underlineWord)
import ComponentTypes (Selection)

data SentenceChoice = PhraseChosen | SentenceChosen

type Props = {route :: {params :: {id :: String}}, navigation :: { navigate :: EffectFn2 String { selection :: Selection, wordTranslation :: String, rangeTranslation :: String, range :: String, rangeOffset :: Int, word :: String, existingSentence :: Boolean, audio :: Maybe String } Unit }}

type Query
  = { selectedSnippet :: Selection }
query =
  gql
    """
  query getSelections($snippetId: ID) {
    selectedSnippet(id: $snippetId) {
      id
      word
      sentence
      phrase
      sentenceOffset
      phraseOffset
      wordTranslation
      phraseTranslation
      sentenceTranslation
      wordLength
      book {
        id
        language
      }
    }
  }
"""

numberOne p = element listIcon $ unsafeUnion p { color: "#000", icon: "numeric-1-box" }
numberTwo p = element listIcon $ unsafeUnion p { color: "#000", icon: "numeric-2-box" }
reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "SentenceChoice" $ buildJsx

sentenceListItem range offset word translation redirect leftIcon = listItem {
    title: underlineWord range offset word (M.css {fontWeight: "bold"}) "bold" 16,
    titleNumberOfLines: 5,
    descriptionNumberOfLines: 5,
    description: translation,
    left: leftIcon,
    onPress: RNE.capture_ $ redirect
  }
buildJsx props = React.do
  result <- useData (Proxy :: Proxy Query) query {variables: {snippetId: props.route.params.id}, fetchPolicy: "cache-and-network"}
  case result.state of
       Nothing -> mempty
       Just {selectedSnippet: selection} -> pure $ M.getJsx do
          M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
            surface { style: M.css { flex: 1 } } do
              M.view {style: M.css {alignItems: "center", marginBottom: 10, marginTop: 10}} do
                caption {style: M.css {}} $ M.string "Choose the sentence length you wish to appear on your flashcard"
              divider {style: M.css {height: 1, width: "100%"}}
              if length selection.phrase /= length selection.sentence
                then sentenceListItem selection.phrase selection.phraseOffset selection.word selection.phraseTranslation (redirect selection PhraseChosen) numberOne else mempty
              divider {style: M.css {height: 1, width: "100%"}}
              sentenceListItem selection.sentence selection.sentenceOffset selection.word selection.sentenceTranslation (redirect selection SentenceChosen) (if length selection.phrase /= length selection.sentence then numberTwo else numberOne)
              divider {style: M.css {height: 1, width: "100%"}}
  where redirect selection SentenceChosen =
          runEffectFn2 props.navigation.navigate "ImageChoice" $
            { selection: selection
            , range: selection.sentence
            , rangeTranslation: selection.sentenceTranslation
            , wordTranslation: selection.wordTranslation
            , rangeOffset: selection.sentenceOffset
            , word: selection.word
            , existingSentence: false
            , audio: Nothing
            }
        redirect selection PhraseChosen =
          runEffectFn2 props.navigation.navigate "ImageChoice" $
            { selection: selection
            , range: selection.phrase
            , rangeTranslation: selection.phraseTranslation
            , wordTranslation: selection.wordTranslation
            , rangeOffset: selection.phraseOffset
            , word: selection.word
            , existingSentence: false
            , audio: Nothing
            }
