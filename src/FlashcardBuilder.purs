module FlashcardBuilder where

import Prelude
import ImageSearch (imageSearch, Image)
import Paper (textInput, surface, button, title, divider, listItem)
import React.Basic.Hooks as React
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import Data.Either (Either(..))
import Effect.Uncurried (runEffectFn1, EffectFn1)
import Effect.Aff (Aff, launchAff_, try)
import React.Basic.Events (EventFn, unsafeEventFn)
import Unsafe.Coerce (unsafeCoerce)
import Effect.Class (liftEffect)
import Data.Undefinable (toUndefinable)
import Data.Maybe (Maybe(..))
import Markup as M
import Data.Foldable (foldl)
import Effect.Exception (message)
import Context (dataStateContext, Context)
import Image (_image)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Dimensions (window)
import Debug.Trace (spy)
import Data.String (length)
import React.Basic.Native as RN
import Paper (title)
import Data.String (split, Pattern(..), trim)
import Config (config)
import Translator (translate)
import Record.Unsafe (unsafeGet)

type Selection = {word :: String, sentence :: String, phrase :: String, phraseOffset :: Int, sentenceOffset :: Int, book :: {language :: String}}
type Props = {route :: {params :: {selection :: Selection}}}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "FlashcardBuilder" $ buildJsx

text :: EventFn (RNE.NativeSyntheticEvent String) String
text = unsafeEventFn \e -> (unsafeCoerce e)

changeField setField =
  RNE.handler text \t ->
    setField \_ -> t

result i = pure $ element _image {style: M.css {height: window.width / 3, width: window.width / 3}, source: {uri: spy "URI" i.item.link}}

getImages keyword setImages setError = launchAff_ do
  liftEffect $ setImages \_ -> []
  result <- try $ imageSearch keyword 0 5
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ spy "ERROR" $ message error
    Right images -> liftEffect $ setImages \_ -> images

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
                  textList: textResult {textDecorationLine: "underline"} word textList,
                  sentenceLength: sentenceLength + length word + 1
                }
            | otherwise =
                  {
                    textList: textResult {} word textList,
                    sentenceLength: sentenceLength + length word + 1
                  }

sentenceListItem sentence offset translation = listItem {
    title: underlineWord sentence offset,
    titleNumberOfLines: 4,
    descriptionNumberOfLines: 4,
    description: translation
  }
buildJsx props = React.do
  let selection = props.route.params.selection
  keyword /\ setKeyword <- useState props.route.params.selection.word
  sentenceTranslation /\ setSentenceTranslation <- useState ""
  phraseTranslation /\ setPhraseTranslation <- useState ""
  images /\ setImages <- useState ([] :: Array Image)
  { setLoading, setError } <- useContext dataStateContext
  let translateWithConfig = translate (unsafeGet "CSE_API_KEY" config) selection.book.language
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
  --useEffect selection.word do
    --getImages selection.word setImages setError
    --pure mempty

  pure $ M.getJsx do
    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      surface { style: M.css { flex: 1 } } do
        title {} $ M.string selection.word
        divider {style: M.css {height: 1, width: "100%"}}
        if length selection.phrase /= length selection.sentence
          then sentenceListItem selection.phrase selection.phraseOffset phraseTranslation else mempty
        divider {style: M.css {height: 1, width: "100%"}}
        sentenceListItem selection.sentence selection.sentenceOffset sentenceTranslation
        divider {style: M.css {height: 1, width: "100%"}}
        M.flatList {data: images, renderItem: mkEffectFn1 result, style: M.css {flex: 1}, numColumns: 3.0}
