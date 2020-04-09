module FlashcardBuilder.SentenceChoice where

import Prelude
import Paper (textInput, surface, button, title, divider, listItem)
import React.Basic.Hooks as React
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Effect.Aff (Aff, launchAff_, try)
import React.Basic.Events (EventFn, unsafeEventFn)
import Unsafe.Coerce (unsafeCoerce)
import Effect.Class (liftEffect)
import Data.Undefinable (toUndefinable)
import Data.Maybe (Maybe(..))
import Markup as M
import Data.Foldable (foldl)
import Effect.Exception (message)
import Image (_image)
import Data.String (length)
import React.Basic.Native as RN
import Paper (title)
import Data.String (split, Pattern(..), trim)
import Config (config)
import Translator (translate)
import Record.Unsafe (unsafeGet)

type Selection = {word :: String, sentence :: String, phrase :: String, phraseOffset :: Int, sentenceOffset :: Int, book :: {language :: String}}
type Props = {route :: {params :: {selection :: Selection}}, navigation :: { navigate :: EffectFn2 String { selection :: Selection, wordTranslation :: String, rangeTranslation :: String, range :: String } Unit }}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "SentenceChoice" $ buildJsx

underlineWord sentence offset = _.textList $ foldl foldFn accumStart words
  where words = split (Pattern " ") sentence
        accumStart = {textList: mempty, sentenceLength: 0}
        textResult :: forall a . Record a -> String -> JSX -> JSX
        textResult style word list = list <>
                                     (M.getJsx $ M.text {style: M.css style} $ M.string word)
                                     <> (M.getJsx $ M.text {style: M.css style} $ M.string " ")
        foldFn {textList, sentenceLength} word
            | (sentenceLength + length word) > offset && sentenceLength < offset =
                {
                  textList: textResult {textDecorationLine: "underline", fontWeight: "bold"} word textList,
                  sentenceLength: sentenceLength + length word + 1
                }
            | otherwise =
                  {
                    textList: textResult {fontWeight: "bold"} word textList,
                    sentenceLength: sentenceLength + length word + 1
                  }

sentenceListItem range offset translation redirect = listItem {
    title: underlineWord range offset,
    titleNumberOfLines: 5,
    descriptionNumberOfLines: 5,
    description: translation,
    onPress: RNE.capture_ $ redirect range translation
  }
buildJsx props = React.do
  let selection = props.route.params.selection
  let redirect = redirectFn selection
  keyword /\ setKeyword <- useState props.route.params.selection.word
  wordTranslation /\ setWordTranslation <- useState ""
  sentenceTranslation /\ setSentenceTranslation <- useState ""
  phraseTranslation /\ setPhraseTranslation <- useState ""
  let translateWithConfig = translate (unsafeGet "CSE_API_KEY" config) selection.book.language

  useEffect selection.word do
    launchAff_ do
     result <- translateWithConfig selection.word
     liftEffect $ setWordTranslation \_ -> result
    pure mempty

  useEffect selection.sentence do
    launchAff_ do
     result <- translateWithConfig selection.sentence
     liftEffect $ setSentenceTranslation \_ -> result
    pure mempty

  useEffect selection.phrase do
    launchAff_ do
      result <- translateWithConfig selection.phrase
      liftEffect $ setPhraseTranslation \_ -> result
    pure mempty
  
  pure $ M.getJsx do
    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      surface { style: M.css { flex: 1 } } do
        listItem {title: M.getJsx $ M.text {} $ M.string wordTranslation}
        divider {style: M.css {height: 1, width: "100%"}}
        if length selection.phrase /= length selection.sentence
          then sentenceListItem selection.phrase selection.phraseOffset phraseTranslation (redirect wordTranslation) else mempty
        divider {style: M.css {height: 1, width: "100%"}}
        sentenceListItem selection.sentence selection.sentenceOffset sentenceTranslation (redirect wordTranslation)
        divider {style: M.css {height: 1, width: "100%"}}
  where redirectFn selection wordTranslation range rangeTranslation =
          runEffectFn2 props.navigation.navigate "ImageChoice" $
            { selection: selection
            , range: range
            , rangeTranslation: rangeTranslation
            , wordTranslation: wordTranslation
            }
