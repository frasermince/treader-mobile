module FlashcardBuilder.ImageChoice where

import Prelude
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab)
import ImageSearch (imageSearch, Image)
import Markup as M
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Effect.Unsafe (unsafePerformEffect)
import Image (image)
import Dimensions (window)
import Debug.Trace (spy)
import Context (dataStateContext, Context)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, mkEffectFn2)
import Effect.Exception (message)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Data.Either (Either(..))
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import React.Basic.Events (EventFn, unsafeEventFn)
import Unsafe.Coerce (unsafeCoerce)
import FlashcardBuilder.Util(underlineWord)
import Data.Array (replicate, modifyAt, (!!))
import Data.Int (floor)
import Data.Maybe (fromMaybe)
import MaterialIcon (icon)
import TextToSpeech (speak, setDefaultLanguage)
import Data.Map (fromFoldable, lookup)
import Data.Traversable (traverse_)

type Selection = {word :: String, sentence :: String, phrase :: String, phraseOffset :: Int, sentenceOffset :: Int, book :: {language :: String}}

type Props = {route :: {params :: {selection :: Selection, selection :: Selection, range :: String, wordTranslation :: String, rangeTranslation :: String, rangeOffset :: Int}}}

text :: EventFn (RNE.NativeSyntheticEvent String) String
text = unsafeEventFn \e -> (unsafeCoerce e)

languageList = fromFoldable [
  ("en" /\ "en-US"),
  ("es" /\ "es-MX"),
  ("fr" /\ "fr-FR"),
  ("de" /\ "de-DE"),
  ("it" /\ "it-IT")
]

changeField setField =
  RNE.handler text \t ->
    setField \_ -> t

getImages setSelected keyword setImages setError = launchAff_ do
  liftEffect $ setSelected \_ -> replicate 8 false
  liftEffect $ setImages \_ -> []
  result <- try $ imageSearch keyword 0 8
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ message error
    Right images -> liftEffect $ setImages \_ -> images

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "ImageChoice" $ buildJsx

imageStyle isSelected = {height: window.width / 4.5, width: window.width / 4.5, margin: "1.5%", borderColor: "#66aab1", borderWidth: if isSelected then 1 else 0}

determineSelection setSelected index =
  setSelected \array -> fromMaybe array $ modifyAt index not array

selectableImage selected setSelected i = pure $ M.getJsx do
  let index = floor i.index
  let isSelected = fromMaybe false $ selected !! index
  M.touchableOpacity {onPress: RNE.capture_ $ determineSelection setSelected index} do
    if isSelected then badge {style: M.css {position: "absolute", zIndex: 8, top: -6, right: 1, backgroundColor: "#66aab1" }} $ icon {color: "white", name: "check", size: 14} else mempty
    image {style: M.css $ imageStyle isSelected, source: {uri: i.item.image.thumbnailLink}}

buildJsx props = React.do
  { setLoading, setError } <- useContext dataStateContext
  let params = props.route.params
  let selection = params.selection
  selected /\ setSelected <- useState $ replicate 8 false
  search /\ setSearch <- useState selection.word
  images /\ setImages <- useState ([] :: Array Image)
  showSearch /\ setShowSearch <- useState false
  useEffect selection.book.language do
    traverse_ setDefaultLanguage $ lookup selection.book.language languageList
    pure mempty
  useEffect selection.word do
    getImages setSelected selection.word setImages setError
    pure mempty

  pure $ M.getJsx do
    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      surface { style: M.css { flex: 1 } } do
         M.view {style: M.css {flex: 2, justifyContent: "flex-end", marginLeft: 15}} do
           headline {} $ M.string selection.word
           M.text {style: M.css{marginBottom: 30}} $ M.string params.wordTranslation
         M.view {style: M.css {flex: 6}} do
           M.view {style: M.css {flex: 1}} do
            divider {style: M.css {height: 1, width: "100%"}}
            M.view {style: M.css {paddingTop: 40, paddingLeft: 5, paddingRight: 5}} do
              fab {icon: "volume-medium", small: true, style: M.css {width: 40}, onPress: RNE.capture_ $ speak params.range {}}
              paragraph {} $ M.jsx $ [ underlineWord params.range params.rangeOffset]
            paragraph {style: M.css {paddingTop: 20, paddingLeft: 5, paddingRight: 5}} $ M.string $ params.rangeTranslation
           M.view {style: M.css {flex: 3, justifyContent: "flex-end"}} do
            if showSearch then do
              fab {icon: "close", small: true, style: M.css {width: 40, alignSelf: "flex-end"}, onPress: RNE.capture_ $ setShowSearch \show -> not show}
              textInput {label: "Search", onChangeText: changeField setSearch, value: search, style: M.css {marginTop: 40} }
              button { onPress: RNE.capture_ $ getImages setSelected search setImages setError } $ M.string "Search"
            else fab {icon: "magnify", small: true, style: M.css {width: 40, alignSelf: "flex-end", marginTop: 130}, onPress: RNE.capture_ $ setShowSearch \show -> not show}
            M.flatList {
              data: images,
              renderItem: mkEffectFn1 $ selectableImage selected setSelected,
              keyExtractor: mkEffectFn2 \i n -> pure i.link,
              style: M.css {flex: 2},
              contentContainerStyle: M.css {flex: 2, justifyContent: "flex-end"}, numColumns: 4.0
            }
            button { mode: "contained", onPress: RNE.capture_ $ mempty} $ M.string "Add Images"
