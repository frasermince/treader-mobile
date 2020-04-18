module FlashcardReview.CardItem where

import Prelude
import React.Basic.Hooks as React
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import Effect (Effect)
import StackSwiper (cardStack, card)
import Dimensions (window)
import Markup as M
import Data.Array (head)
import Effect.Unsafe (unsafePerformEffect)
import Data.Maybe (fromMaybe)
import Image (image)
import Data.Foldable (foldl)
import Data.Array (length)
import Data.Int (toNumber)
import Dimensions (window)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Data.Traversable (traverse_)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import Debug.Trace (spy)
import BindThis (bindThis)

gray = "#757E90"
containerCardItem = {
  backgroundColor: "white",
  borderRadius: 8,
  alignItems: "center",
  margin: 10,
  shadowOpacity: 0.05,
  shadowRadius: 10,
  height: window.height * 0.75,
  shadowColor: "black",
  shadowOffset: { height: 0, width: 0 },
  padding: 10
}

imageStyle imageCount = {
  borderRadius: 8,
  flex: 1,
  width: (window.width / imageCount) - 50.0,
  height: (window.width / imageCount) - 50.0
}

nameStyle = {
  paddingTop: 25,
  paddingBottom: 7,
  color: "#363636",
  fontSize: 30
}

descriptionCardItem = {
  color: gray,
  textAlign: "center"
}

type Props = {word :: String, description :: String, imageUrl :: Array String, onPressLeft :: Effect Unit, onPressRight :: Effect Unit, index :: Int}

imageJsx imageCount accum imageUrl = accum <> image {style: M.css $ imageStyle $ toNumber imageCount, source: {uri: imageUrl}}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "CardItem" $ buildJsx

buildJsx props = React.do
  flipRef <- useRef null
  let flip = do
        result <- readRefMaybe flipRef
        traverse_ (\s -> bindThis s.flipY s) (spy "RESULT" result)

  pure $ M.getJsx do
     card {index: props.index, style: M.css {width: window.width}, ref: flipRef} do
        M.touchableOpacity {style: M.css containerCardItem, onPress: RNE.capture_ $ flip} do
            M.text {style: M.css nameStyle} $ M.string props.word
            M.text {style: M.css descriptionCardItem} $ M.string props.description
            M.view {style: M.css {flexDirection: "column", flex: 1, width: window.width - 60.0, justifyContent: "flex-end"}} do
              M.view {style: M.css {flexDirection: "row"}} do
                foldl (imageJsx $ length props.imageUrl) mempty props.imageUrl
        M.view {style: M.css containerCardItem} do
            M.text {style: M.css nameStyle} $ M.string props.word
            M.text {style: M.css descriptionCardItem} $ M.string props.description
            M.view {style: M.css {flexDirection: "column", flex: 1, width: window.width - 60.0, justifyContent: "flex-end"}} do
              M.view {style: M.css {flexDirection: "row"}} do
                foldl (imageJsx $ length props.imageUrl) mempty props.imageUrl
