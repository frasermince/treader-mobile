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
import Effect.Console (log)
import Image (image)
import Data.Foldable (foldl)
import Data.Array (length)
import Data.Int (toNumber)
import Dimensions (window)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Data.Traversable (traverse_, traverse)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import BindThis (bindThis)
import FlashcardBuilder.Util (clozeWord, underlineWordMarkup)
import FetchBlob (fetch)
import Effect.Aff (Aff, launchAff_, try)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Class (liftEffect)
import FS (audioDir, writeFile, exists, unlink, absintheFile)
import Debug.Trace (spy)
import Sound (play, Sound, release, stop, createSound, stopAndPlay)
import Data.String (stripPrefix, Pattern(..))
import Global (encodeURIComponent)

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
  width: ((window.width - (window.width / 2.0)) / imageCount),
  height: ((window.width - (window.width / 2.0)) / imageCount)
}

nameStyle = {
  paddingTop: 25,
  paddingBottom: 7,
  color: "#363636",
  fontSize: 30
}

promptStyle = {
  paddingTop: 5,
  color: "#363636",
  fontSize: 20,
  flex: 1
}

descriptionCardItem = {
  textAlign: "center",
  fontSize: 20
}

type Props = {active :: Boolean, word :: String, sentence :: String, offset :: Int, imageUrl :: Array String, onPressLeft :: Effect Unit, onPressRight :: Effect Unit, index :: Int, audioUrl :: String, sentenceId :: String, setIsFlipped :: (Boolean -> Boolean) -> Effect Unit, isFlipped :: Boolean}

imageJsx imageCount accum imageUrl = accum <> image {style: M.css $ imageStyle $ toNumber imageCount, source: {uri: imageUrl}}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "CardItem" $ buildJsx

fileExists :: Maybe String -> Aff Boolean
fileExists (Just filePath) = exists filePath
fileExists Nothing = pure false

flipEnd setIsFlipped sound = launchAff_ do
  liftEffect $ setIsFlipped \_ -> true
  traverse_ stopAndPlay sound

buildJsx props = React.do
  audioPath /\ setAudioPath <- useState (Nothing :: Maybe String)
  sound /\ setSound <- useState (Nothing :: Maybe Sound)
  flipRef <- useRef null
  let flip = do
        result <- readRefMaybe flipRef
        traverse_ (\s -> bindThis s.flipY (spy "FLIP RESULT" s)) result

  let setAudioInformation :: String -> Aff Sound
      setAudioInformation path = do
        s <- createSound path
        liftEffect $ setAudioPath \_ -> Just path
        liftEffect $ setSound \_ -> Just $ s
        pure $ spy "RESULT" s

  useEffect props.active do
     props.setIsFlipped \_ -> false
     launchAff_ $ traverse_ stop sound
     pure mempty
  useEffect (props.audioUrl /\ props.index) do
     launchAff_ do
        let file = audioDir <> "/sentence-" <> props.sentenceId <> ".mp3"
        e <- exists $ spy "EXIST CHECK" file
        case spy "EXISTS" e of
          true -> traverse_ setAudioInformation (encodeURIComponent file)
          false -> do
            result <- fetch {fileCache: true, path: spy "FILE" file} "GET" props.audioUrl {}
            path <- liftEffect $ spy "PATH" result.path
            traverse_ setAudioInformation (encodeURIComponent path)
     pure $ launchAff_ do
        e <- fileExists audioPath
        if e then unlink $ fromMaybe "" audioPath else mempty
        traverse_ stop sound
        liftEffect $ traverse_ release sound
  
  pure $ M.getJsx do
     card {index: props.index, style: M.css {width: window.width}, ref: flipRef, onFlipEnd: flipEnd props.setIsFlipped sound, key: props.active} do
        M.touchableOpacity {style: M.css containerCardItem, onPress: RNE.capture_ $ flip} do
            M.text {style: M.css promptStyle} $ M.string "What word goes in the blank"
            M.text {style: M.css {flexWrap:"wrap", flexDirection: "row", marginRight: 5, marginLeft: 5, flex: 4}} do
               clozeWord (spy "SENTENCE" props.sentence) props.offset props.word $ M.css descriptionCardItem
            M.view {style: M.css {flexDirection: "column", flex: 1, width: window.width - 60.0, justifyContent: "flex-end"}} do
              M.view {style: M.css {flexDirection: "row"}} do
                foldl (imageJsx $ length props.imageUrl) mempty props.imageUrl
        M.view {style: M.css containerCardItem} do
            M.text {style: M.css promptStyle} $ M.string props.word
            M.text {style: M.css {marginRight: 5, marginLeft: 5, flex: 2}} do
               underlineWordMarkup props.sentence props.offset props.word (M.css descriptionCardItem) "normal" 20
            M.view {style: M.css {flex: 3}} do
              fab {icon: "volume-medium", small: true, style: M.css {width: 40}, onPress: RNE.capture_ $ launchAff_ $ traverse_ stopAndPlay sound}
            M.view {style: M.css {flexDirection: "column", flex: 1, width: window.width - 60.0, justifyContent: "flex-end"}} do
              M.view {style: M.css {flexDirection: "row"}} do
                foldl (imageJsx $ length props.imageUrl) mempty props.imageUrl
