module FlashcardBuilder.ImageChoice where

import Prelude
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline)
import ImageSearch (imageSearch, Image)
import Markup as M
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Effect.Unsafe (unsafePerformEffect)
import Image (_image)
import Dimensions (window)
import Debug.Trace (spy)
import Context (dataStateContext, Context)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1)
import Effect.Exception (message)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Data.Either (Either(..))
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import React.Basic.Events (EventFn, unsafeEventFn)
import Unsafe.Coerce (unsafeCoerce)
import FlashcardBuilder.Util(underlineWord)

type Selection = {word :: String, sentence :: String, phrase :: String, phraseOffset :: Int, sentenceOffset :: Int, book :: {language :: String}}

type Props = {route :: {params :: {selection :: Selection, selection :: Selection, range :: String, wordTranslation :: String, rangeTranslation :: String, rangeOffset :: Int}}}

text :: EventFn (RNE.NativeSyntheticEvent String) String
text = unsafeEventFn \e -> (unsafeCoerce e)

changeField setField =
  RNE.handler text \t ->
    setField \_ -> t

getImages keyword setImages setError = launchAff_ do
  liftEffect $ setImages \_ -> []
  result <- try $ imageSearch keyword 0 8
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ spy "ERROR" $ message error
    Right images -> liftEffect $ setImages \_ -> images

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "ImageChoice" $ buildJsx

result i = pure $ element _image {style: M.css {height: window.width / 4.5, width: window.width / 4.5, margin: "1.5%"}, source: {uri: spy "URI" i.item.link}}

buildJsx props = React.do
  { setLoading, setError } <- useContext dataStateContext
  let params = props.route.params
  let selection = params.selection
  search /\ setSearch <- useState selection.word
  images /\ setImages <- useState ([] :: Array Image)
  useEffect selection.word do
    getImages selection.word setImages setError
    pure mempty

  pure $ M.getJsx do
    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      surface { style: M.css { flex: 1 } } do
         M.view {style: M.css {flex: 2, justifyContent: "flex-end", marginLeft: 15}} do
           headline {} $ M.string selection.word
           M.text {style: M.css{marginBottom: 30}} $ M.string params.wordTranslation
         M.view {style: M.css {flex: 6}} do
           divider {style: M.css {height: 1, width: "100%"}}
           paragraph {} $ M.jsx $ [ underlineWord params.range params.rangeOffset]
           paragraph {} $ M.string $ params.rangeTranslation
           textInput {label: "Search", onChangeText: changeField setSearch, value: search }
           button { mode: "contained", onPress: RNE.capture_ $ getImages search setImages setError } $ M.string "Search"
           M.flatList {data: images, renderItem: mkEffectFn1 result, style: M.css {flex: 1}, numColumns: 4.0}
           button { mode: "contained", onPress: RNE.capture_ $ mempty} $ M.string "Add Images"
