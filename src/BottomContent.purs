module BottomContent where

import Prelude
import Effect.Aff (Aff, launchAff_)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Native.Events as RNE
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import Effect.Class (liftEffect)
import React.Basic.Hooks as React
import React.Basic.Native as RN
import Animated (scrollView, timing, value, getNode, scrollTo)
import Effect.Unsafe (unsafePerformEffect)
import Record as Record
import Platform as Platform
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Slider (slider)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Markup as M
import Paper (surface)
import Record (get, merge)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Foreign.Object (lookup, Object, fold)
import Morphology (valueNames)
import EpubUtil (HighlightedContent)
import Dimensions (window)
import Data.Int (floor)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..), try)
import Control.Alt ((<|>))
import Data.Either (Either(..))
import ApolloHooks (useMutation, gql)
import React.Basic.Native.Events (capture_)
import Foreign.Object (Object)
import TranslatableOnPress as TranslatableOnPress
import Data.String (length)
import Blur (blurView)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Navigation (useNavigation)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Data.Traversable (traverse_)
import ComponentTypes

mapValue :: String -> String -> String
mapValue "infinitive" value = value

mapValue key value = fromMaybe value $ lookup value $ valueNames

mutation =
  gql
    """
  mutation translateMutation($input: TranslateInput!) {
    translate(input: $input) {
      translation
    }
  }
"""

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "BottomContent") buildJsx

type Props
  = {
    translation :: Maybe {text :: String, isPermitted :: Boolean},
    morphology :: Maybe (Object String),
    wordPlacement :: Maybe Number,
    context :: Maybe Context,
    language :: Maybe String,
    setMorphology :: (Maybe (Object String) -> Maybe (Object String)) -> Effect Unit,
    setTranslation :: (Maybe Translation -> Maybe Translation) -> Effect Unit,
    setModalVisible :: (Boolean -> Boolean) -> Effect Unit,
    removeContent :: Effect Unit
    }

blurTextStyle = {
  position: "absolute",
  top: 0,
  left: 0,
  bottom: 0,
  right: 0,
  justifyContent: "center",
  alignItems: "center",
  height: "100%"
}
blurStyle = {
  position: "absolute",
  top: 0,
  left: 0,
  bottom: 0,
  right: 0,
  justifyContent: "center",
  alignItems: "center"
  }
styles fade =
  { --backgroundColor: "#cdcdcd",
  --paddingTop: 0,
    height: 250
  , right: 0
  , left: 0
  , position: "absolute"
  , alignItems: "center"
  , justifyContent: "center"
  , flexDirection: "row"
  , opacity: fade
  , marginTop: 0
  , zIndex: zIndex
  , shadowOpacity: 0.75
  , shadowRadius: 3
  , shadowOffset: { height: 5, width: 10 }
  }
  where
  zIndex :: Int
  zIndex =
    fade.interpolate
      { inputRange: [ 0, 1 ]
      , outputRange: [ -1, 9 ]
      }

titleStyles =
  M.css
    { fontSize: 15
    , fontWeight: "bold"
    , marginBottom: 5
    }

runAnimation true fade = timing fade { toValue: 1, duration: 20 }

runAnimation false fade = timing fade { toValue: 0, duration: 20 }

shouldBlur translation = not $ fromMaybe true (_.isPermitted <$> translation)
unpermittedBlur props =
    blurView {style: M.css blurStyle, blurType: "light", blurAmount: 5} do
      M.touchableOpacity {style: M.css blurTextStyle, onPress: blurPress} do
        M.text {style: M.css {}} $ M.string "You have used your words for the day. Press to subscribe and receive unlimited words"
  where blurPress = RNE.capture_ do
          props.removeContent
          props.setModalVisible \_ -> true

container fade height (Just wordPlacement) children
  | height - (floor wordPlacement) < 450 = surface {style: M.css $ merge (styles fade) { top: 0}} $ M.view {style: M.css {flex: 1, marginTop: 30}} children
  | otherwise = surface {style: M.css $ merge (styles fade) { bottom: 0 }} children

container fade height wordPlacement children = surface {style: M.css $ merge (styles fade) { bottom: 0 }} children

buildJsx props = React.do
  navigation <- useNavigation
  mutationFn /\ result <- useMutation mutation {}
  fade /\ setFade <- useState $ value 1
  ref <- useRef null
  placementForAnimation /\ setPlacementForAnimation <- useState props.wordPlacement

  useEffect props.wordPlacement do
     result <- readRefMaybe ref
     traverse_ (\s -> scrollTo (getNode s) 0) result
     launchAff_ $ do
        runAnimation visible fade
        liftEffect $ setPlacementForAnimation \_ -> props.wordPlacement
        if visible then pure unit else do
          liftEffect $ props.setTranslation \_ -> Nothing
          liftEffect $ props.setMorphology \_ -> Nothing
     pure mempty
  pure $ M.getJsx
    $ container fade window.height placementForAnimation do
       scrollView { ref: ref, style: M.css { marginLeft: 20, marginRight: 20, paddingTop: 20 }, contentContainerStyle: M.css {flexGrow: 1}, showsVerticalScrollIndicator: false } do
          translationMarker <> fromMaybe translationPlaceholder translationText
          M.view {style: M.css {paddingBottom: 40}} do
            tappableTranslations mutationFn
            maybeDataMap props.morphology
       if shouldBlur props.translation then unpermittedBlur props else mempty
  where
  displaySentence = fromMaybe false do
     context <- props.context
     s <- context.sentence
     pure $ (length s) < 400
  displayPhrase = fromMaybe false do
     context <- props.context
     p <- context.phrase
     surrounding <- context.surrounding
     sentence <- context.sentence
     pure $ (length sentence - length p > 25) && length p < 400

  displaySurrounding = fromMaybe false do
     context <- props.context
     p <- context.phrase
     s <- context.surrounding
     let delta = (length p) - (length s)
     pure $ delta > 20 || delta < -20

  phraseOffset = do
     context <- props.context
     context.sentenceOffset

  sentenceOffset = do
     context <- props.context
     context.sentenceOffset
  sentence = do
     context <- props.context
     context.sentence
  phrase = do
     context <- props.context
     context.phrase
  surrounding = do
     context <- props.context
     context.surrounding

  tappableTranslations mutationFn = do
     if sentence == phrase || displaySurrounding
       then M.childElement TranslatableOnPress.reactComponent {snippet: surrounding, labelText: "Adjacent", mutationFn: mutationFn, language: props.language}
       else mempty
     if sentence /= phrase && displayPhrase
       then M.childElement TranslatableOnPress.reactComponent {snippet: phrase, labelText: "Phrase", mutationFn: mutationFn, language: props.language}
       else mempty
     if displaySentence && not (displayPhrase && displaySurrounding)
       then M.childElement TranslatableOnPress.reactComponent {snippet: sentence, labelText: "Sentence", mutationFn: mutationFn, language: props.language}
       else mempty

  visible = isJust props.wordPlacement

  translationMarker = M.text { style: titleStyles } $ M.string "Translation"

  translationText = (M.text {} <$> M.string <$> _.text <$> props.translation)
  translationPlaceholder = M.text {} $ M.string ""

maybeDataMap :: Maybe (Object String) -> M.Markup Unit
maybeDataMap morphology = fromMaybe mempty (dataMap <$> morphology)
  where
  dataMap d =
    M.view { style: M.css { marginTop: 10 } } do
      M.text { style: titleStyles } $ M.string $ "Word Information"
      fold foldFn (mempty :: M.Markup Unit) d

  foldFn :: forall a. M.Markup Unit -> String -> String -> M.Markup Unit
  foldFn accum "lemma" value = foldView accum "infinitive" value
  foldFn accum "pos" value = foldView accum "Part Of Speech" value
  foldFn accum "gender" value = foldView accum "Gender" value
  foldFn accum "tense" value = foldView accum "Verb Tense" value
  foldFn accum "verbform" value = foldView accum "Verb Form" value
  foldFn accum "number" value = foldView accum "Number" value
  foldFn accum "person" value = foldView accum "Person" value
  foldFn accum "mood" value = foldView accum "Mood" value
  foldFn accum key value = accum <> pure unit

  foldView :: forall a. M.Markup Unit -> String -> String -> M.Markup Unit
  foldView accum key value =
    accum
      <> M.view { style: M.css { flex: 1, alignSelf: "stretch", flexDirection: "row" } } do
          M.view { style: M.css { flex: 1, alignSelf: "stretch" } } $ M.text {} $ M.string key
          M.view { style: M.css { flex: 1, alignSelf: "stretch" } } $ M.text {} $ M.string $ mapValue key value
