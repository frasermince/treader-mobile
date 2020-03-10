module BottomContent where

import Prelude
import Effect.Aff (Aff, launchAff_)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import Effect.Class (liftEffect)
import React.Basic.Hooks as React
import React.Basic.Native as RN
import Animated (scrollView, timing, value)
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
import Debug.Trace (spy)

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
    translation :: Maybe String,
    morphology :: Maybe (Object String),
    wordPlacement :: Maybe Number,
    surrounding :: Maybe String,
    sentence :: Maybe String,
    phrase :: Maybe String,
    language :: Maybe String,
    setMorphology :: (Maybe (Object String) -> Maybe (Object String)) -> Effect Unit,
    setTranslation :: (Maybe String -> Maybe String) -> Effect Unit
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

container fade height (Just wordPlacement) children
  | height - (floor wordPlacement) < 450 = surface {style: M.css $ merge (styles fade) { top: 0}} $ M.view {style: M.css {flex: 1, marginTop: 30}} children
  | otherwise = surface {style: M.css $ merge (styles fade) { bottom: 0 }} children

container fade height wordPlacement children = surface {style: M.css $ merge (styles fade) { bottom: 0 }} children

buildJsx props = React.do
  mutationFn /\ result <- useMutation mutation {}
  fade /\ setFade <- useState $ value 1
  placementForAnimation /\ setPlacementForAnimation <- useState props.wordPlacement

  useEffect props.wordPlacement do
     launchAff_ $ do
        runAnimation visible fade
        liftEffect $ setPlacementForAnimation \_ -> props.wordPlacement
        if visible then pure unit else do
          liftEffect $ props.setTranslation \_ -> Nothing
          liftEffect $ props.setMorphology \_ -> Nothing
     pure mempty
  pure $ M.getJsx
    $ container fade window.height placementForAnimation do
       scrollView { style: M.css { marginLeft: 20, marginRight: 20}, contentContainerStyle: M.css {flexGrow: 1} } do
          fromMaybe mempty $ (append translationMarker) <$> translationText
          tappableTranslations mutationFn
          maybeDataMap props.morphology
  where
  displaySentence = fromMaybe false do
     s <- props.sentence
     pure $ (length s) < 400
  displayPhrase = fromMaybe false do
     p <- props.phrase
     surrounding <- props.surrounding
     sentence <- props.sentence
     pure $ ((spy "SENTENCE" $ length sentence) - (spy "PHRASE" $ length p) > 25) && length p < 400

  displaySurrounding = fromMaybe false do
     p <- props.phrase
     s <- props.surrounding
     let delta = (length p) - (length s)
     pure $ (spy "Delta" delta) > 20 || delta < -20
  tappableTranslations mutationFn = do
     if props.sentence == props.phrase || displaySurrounding
       then M.childElement TranslatableOnPress.reactComponent {snippet: props.surrounding, labelText: "Adjacent", mutationFn: mutationFn, language: props.language}
       else mempty
     if props.sentence /= props.phrase && displayPhrase
       then M.childElement TranslatableOnPress.reactComponent {snippet: props.phrase, labelText: "Phrase", mutationFn: mutationFn, language: props.language}
       else mempty
     if displaySentence && not (displayPhrase && displaySurrounding)
       then M.childElement TranslatableOnPress.reactComponent {snippet: props.sentence, labelText: "Sentence", mutationFn: mutationFn, language: props.language}
       else mempty

  visible = isJust props.wordPlacement

  translationMarker = M.text { style: titleStyles } $ M.string "Translation"

  translationText = (M.text {} <$> M.string <$> props.translation)

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
