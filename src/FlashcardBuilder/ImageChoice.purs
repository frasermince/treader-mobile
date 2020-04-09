module FlashcardBuilder.ImageChoice where

import Prelude
import Paper (textInput, surface, button, title, divider, listItem, paragraph)
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

type Selection = {word :: String, sentence :: String, phrase :: String, phraseOffset :: Int, sentenceOffset :: Int, book :: {language :: String}}

type Props = {route :: {params :: {selection :: Selection, selection :: Selection, range :: String, wordTranslation :: String, rangeTranslation :: String}}}

getImages keyword setImages setError = launchAff_ do
  liftEffect $ setImages \_ -> []
  result <- try $ imageSearch keyword 0 5
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ spy "ERROR" $ message error
    Right images -> liftEffect $ setImages \_ -> images

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "ImageChoice" $ buildJsx

result i = pure $ element _image {style: M.css {height: window.width / 3, width: window.width / 3}, source: {uri: spy "URI" i.item.link}}

buildJsx props = React.do
  { setLoading, setError } <- useContext dataStateContext
  let selection = props.route.params.selection
  images /\ setImages <- useState ([] :: Array Image)
  useEffect selection.word do
    getImages selection.word setImages setError
    pure mempty

  pure $ M.getJsx do
    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      surface { style: M.css { flex: 1 } } do
         title {} $ M.string selection.word
         divider {style: M.css {height: 1, width: "100%"}}
         paragraph {} $ M.string $ props.route.params.range
         paragraph {} $ M.string $ props.route.params.rangeTranslation
         M.flatList {data: images, renderItem: mkEffectFn1 result, style: M.css {flex: 1}, numColumns: 3.0}
