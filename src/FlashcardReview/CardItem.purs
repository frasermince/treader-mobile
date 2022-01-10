module FlashcardReview.CardItem where

import Prelude
import Platform as Platform
import React.Basic.Hooks as React
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import Effect (Effect)
import StackSwiper (card)
import Dimensions (window)
import Markup as M
import Icon (fontAwesomeIcon)
import Data.Array (head)
import Effect.Unsafe (unsafePerformEffect)
import Data.Maybe (fromMaybe)
import Effect.Console (log)
import Image (image, contain, cover)
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
import Effect.Aff (Aff, launchAff_, try, delay, Milliseconds(..))
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import Effect.Class (liftEffect)
import FS (audioDir, writeFile, exists, unlink, absintheFile, readDirectory)
import Sound (play, Sound, release, stop, createSound, stopAndPlay)
import Data.String (stripPrefix, Pattern(..))
import JSURI (encodeURIComponent, decodeURIComponent)
import Blur (blurView, vibrancyView)
import AsyncStorage (getItem, setItem)

blurStyle visible = {
  position: "absolute",
  top: 0,
  left: 0,
  bottom: 0,
  right: 0,
  justifyContent: "center",
  alignItems: "center",
  display: if visible then "flex" else "none"
  }

frontBlurTextStyle = {
  position: "absolute",
  top: 0,
  left: 0,
  bottom: 0,
  right: 0,
  justifyContent: "center",
  alignItems: "center",
  height: "100%",
  fontSize: 18
}

backBlurTextStyle visible = {
  position: if visible then "absolute" else "relative",
  top: 100,
  left: 0,
  bottom: 0,
  right: 0,
  justifyContent: "center",
  alignItems: "center",
  height: "100%",
  fontSize: 18,
  display: if visible then "flex" else "none"
}

gray = "#757E90"
containerCardItem = {
  backgroundColor: "white",
  borderRadius: 8,
  alignItems: "center",
  margin: 10,
  shadowOpacity: 0.05,
  shadowRadius: 10,
  width: window.width * 0.91,
  height: window.height * 0.75,
  shadowColor: "black",
  shadowOffset: { height: 0, width: 0 },
  padding: 10
}

imageStyle imageCount = {
  borderRadius: 8,
  height: "100%",
  marginLeft: if imageCount == 1.0 then "15%" else "4%",
  marginRight: if imageCount == 1.0 then "15%" else "4%"
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

translationCardItem = {
  textAlign: "center",
  marginRight: 5,
  marginLeft: 5,
  flex: 2,
  fontSize: 20
}

descriptionCardItem = {
  textAlign: "center",
  fontSize: 20
}

type Props =
  { active :: Boolean
  , word :: String
  , sentence :: String
  , offset :: Int
  , imageUrl :: Array String
  , onPressLeft :: Effect Unit
  , onPressRight :: Effect Unit
  , index :: Int
  , audioUrl :: String
  , sentenceId :: String
  , setIsFlipped :: (Boolean -> Boolean) -> Effect Unit
  , isFlipped :: Boolean
  , translation :: String
  }

imageJsx imageCount accum imageUrl = accum <> do
  M.view {style: M.css {flex: 1}} do
    image {style: M.css $ imageStyle $ toNumber imageCount, source: {uri: imageUrl}, resizeMode: cover}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        React.reactComponent "CardItem" $ buildJsx

fileExists :: Maybe String -> Aff Boolean
fileExists (Just filePath) = exists filePath
fileExists Nothing = pure false

flipEnd setIsFlipped sound = launchAff_ do
  liftEffect $ setIsFlipped \_ -> true
  traverse_ stopAndPlay sound

backTutorial hasSwipedBefore setTutorialVisible tutorialVisible = do
  Platform.select {ios: vibrancyView $ {style: M.css $ blurStyle tutorialVisible, blurType: "light", blurAmount: 2}, android: mempty}
  M.touchableOpacity {style: M.css $ backBlurTextStyle tutorialVisible, onPressIn: blurPress} do
    M.view {style: M.css {flexDirection: "row", alignItems: "center"}} do
      M.view {style: M.css {flex: 1, alignItems: "center"}} do
        fontAwesomeIcon {name: "angle-left", size: 18}
      M.view {style: M.css {flex: 1, alignItems: "center"}} do
        fontAwesomeIcon {name: "hand-pointer-o", size: 18}
      M.view {style: M.css {flex: 1, alignItems: "center"}} do
        fontAwesomeIcon {name: "angle-right", size: 18}
    M.text {style: M.css {marginTop: 10}} $ M.string "Swipe to continue"

  where blurPress = RNE.capture_ do
          setTutorialVisible \_ -> false

buildJsx props = React.do
  audioPath /\ setAudioPath <- useState (Nothing :: Maybe String)
  sound /\ setSound <- useState (Nothing :: Maybe Sound)
  showTranslation /\ setShowTranslation <- useState false
  hasSwipedBefore /\ setHasSwipedBefore <- useState true
  tutorialVisible /\ setTutorialVisible <- useState $ false
  flipRef <- useRef null
  let flip = do
        result <- readRefMaybe flipRef
        traverse_ (\s -> bindThis s.flipY s) result

  let setAudioInformation :: String -> Aff Sound
      setAudioInformation path = do
        s <- createSound $ path
        liftEffect $ setAudioPath \_ -> Just path
        liftEffect $ setSound \_ -> Just $ s
        pure $ s

  useEffect (props.active /\ sound) do
     if not props.active then
        props.setIsFlipped \_ -> false
     else mempty
     pure $ launchAff_ do
        liftEffect $ log "STOP SOUND NO TRANSLATION"
        liftEffect $ setShowTranslation \_ -> false
        traverse_ stop $ sound

  useEffect audioPath do
     pure unit
     pure $ launchAff_ do
        liftEffect $ log "RELEASE SOUND"
        liftEffect $ traverse_ release sound
        let decodedPath = audioPath >>= decodeURIComponent
        e <- fileExists $ decodedPath
        if e then unlink $ fromMaybe "" decodedPath else mempty

  useEffect (props.active /\ props.index) do
    launchAff_ do
       item <- getItem "HasSwiped"
       case item of
            Just i -> liftEffect $ setHasSwipedBefore \_ -> i == "true"
            Nothing -> liftEffect $ setHasSwipedBefore \_ -> false
    pure mempty

  useEffect (tutorialVisible /\ props.active /\ hasSwipedBefore /\ props.index) do
    let hideTutorial = hasSwipedBefore || props.index /= 0
    case tutorialVisible of
      true -> if hideTutorial then setTutorialVisible \_ -> false else mempty
      false -> if hideTutorial
                 then mempty else launchAff_ do
                    delay $ Milliseconds 5000.0
                    liftEffect $ setTutorialVisible \_ -> true
    pure mempty

  useEffect (props.audioUrl /\ props.index) do
     launchAff_ do
        let file = audioDir <> "/sentence_" <> props.sentenceId <> ".mp3"
        result <- fetch {fileCache: true, path: file} "GET" props.audioUrl {}
        path <- liftEffect result.path
        traverse_ setAudioInformation $ Platform.select {ios: encodeURIComponent path, android: Just path}
     pure mempty
  let toggleTranslation = RNE.capture_ $ setShowTranslation \t -> not t

  pure $ M.getJsx do
     card {index: props.index, style: M.css {width: window.width}, ref: flipRef, onFlipEnd: flipEnd props.setIsFlipped sound, key: props.active} do
        M.touchableOpacity {style: M.css containerCardItem, onPress: RNE.capture_ $ flip} do
            M.text {style: M.css promptStyle} $ M.string "What word goes in the blank"
            M.text {style: M.css {flexWrap: "wrap", flexDirection: "row", marginRight: 5, marginLeft: 5, flex: 5}} do
               clozeWord props.sentence props.offset props.word $ M.css descriptionCardItem
            M.view {style: M.css {flexDirection: "column", flex: 2, width: window.width - 60.0, justifyContent: "flex-end"}} do
              M.view {style: M.css {flexDirection: "row"}} do
                foldl (imageJsx $ length props.imageUrl) mempty props.imageUrl
        M.view {style: M.css containerCardItem} do
            M.text {style: M.css promptStyle} $ M.string props.word
            iconButton {style: M.css {position: "absolute", top: 7, right: 0},icon: "google-translate", size: 20, onPress: toggleTranslation}
            if showTranslation
              then M.text {style: M.css translationCardItem, onPress: toggleTranslation} $ M.string props.translation
              else M.text {style: M.css {marginRight: 5, marginLeft: 5, flex: 3}, onPress: toggleTranslation} do
                 underlineWordMarkup props.sentence props.offset props.word (M.css descriptionCardItem) "normal" 20
            M.view {style: M.css {flex: 3}} do
              fab {icon: "volume-medium", small: true, style: M.css {width: 40}, onPress: RNE.capture_ $ launchAff_ $ traverse_ stopAndPlay sound}
            M.view {style: M.css {flexDirection: "column", flex: 2, width: window.width - 60.0, justifyContent: "flex-end"}} do
              M.view {style: M.css {flexDirection: "row"}} do
                foldl (imageJsx $ length props.imageUrl) mempty props.imageUrl
            M.view {style: M.css {flexDirection: "row", flex: 1, width: window.width - 60.0, justifyContent: "space-between", display: "flex"}} do
              fab {icon: "close", small: true, style: M.css {marginTop: 20, width: 40, height: 40, backgroundColor: "red"}, onPress: RNE.capture_ $ props.onPressLeft }
              fab {icon: "check", small: true, style: M.css {marginTop: 20, width: 40, height: 40}, onPress: RNE.capture_ $ props.onPressRight }