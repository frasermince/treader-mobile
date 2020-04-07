module FlashcardBuilder where

import Prelude
import ImageSearch (imageSearch, Image)
import Paper (textInput, surface, button, title)
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
import Image (image)
import Debug.Trace (spy)

type Props = {}

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

result accum i = accum <> image {style: M.css {height: 50, width: 50}, source: {uri: spy "URI" i.link}}

press keyword setImages setError = launchAff_ do
  liftEffect $ setImages \_ -> []
  result <- try $ imageSearch keyword 0 5
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ spy "ERROR" $ message error
    Right images -> liftEffect $ setImages \_ -> images

buildJsx props = React.do
  keyword /\ setKeyword <- useState ""
  images /\ setImages <- useState ([] :: Array Image)
  { setLoading, setError } <- useContext dataStateContext
  pure $ M.getJsx do
    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      surface { style: M.css { flex: 1 } } do
        textInput { label: "Image Search", onChangeText: changeField setKeyword, value: keyword }
        button { mode: "contained", onPress: RNE.capture_ $ press keyword setImages setError} $ M.string "Search"
        M.scrollView {style: M.css {flex: 1}} do
          foldl result mempty images
