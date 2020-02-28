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
    sentence :: Maybe String,
    phrase :: Maybe String,
    language :: Maybe String
    }

styles fade =
  { --backgroundColor: "#cdcdcd",
  --paddingTop: 0,
  height: 200
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

--buildJsx :: Props -> JSX
sentenceSection translation setTranslation sentence mutationFn language showTranslation setShowTranslation titleName =
  M.touchableOpacity { style: M.css { marginTop: 10 }, onPress: capture_ $ press sentence language translation } do
    fromMaybe mempty $ if showTranslation then (translationElement <|> textElement) else textElement
  where
  translationElement = (append translationMarker) <$> translationText

  textElement = (append marker) <$> sentenceText

  translationMarker = M.text { style: titleStyles } $ M.string $ titleName <> " Translation"

  marker = M.text { style: titleStyles } $ M.string $ titleName <> " (click to translate)"

  sentenceText = (M.text {} <$> M.string <$> sentence)

  translationText = (M.text {} <$> M.string <$> translation)

  press (Just sentence) (Just language) Nothing =
    launchAff_
      $ do
          let
            payload = { variables: { input: { snippet: sentence, language: language } } }
          response <- try $ mutationFn payload
          case response of
            Left err -> pure unit
            Right result -> do
              liftEffect $ setTranslation \_ -> Just result.translate.translation

  press _ _ (Just translation) = do
    setShowTranslation \t -> not t

  press _ _ _ = do
    pure unit

container fade height (Just wordPlacement) children
  | height - (floor wordPlacement) < 400 = surface {style: M.css $ merge (styles fade) { top: 0}} $ M.view {style: M.css {flex: 1, marginTop: 30}} children
  | otherwise = surface {style: M.css $ merge (styles fade) { top: height - 290 }} children

container fade height wordPlacement children = surface {style: M.css $ merge (styles fade) { bottom: 0 }} children

buildJsx props = React.do
  mutationFn /\ result <- useMutation mutation {}
  fade /\ setFade <- useState $ value 1
  sentenceTranslation /\ setSentenceTranslation <- useState $ (Nothing :: Maybe String)
  showSentenceTranslation /\ setShowSentenceTranslation <- useState true
  phraseTranslation /\ setPhraseTranslation <- useState $ (Nothing :: Maybe String)
  showPhraseTranslation /\ setShowPhraseTranslation <- useState true
  useEffect props.sentence do
    setSentenceTranslation \_ -> Nothing
    setShowSentenceTranslation \_ -> true
    setPhraseTranslation \_ -> Nothing
    setShowPhraseTranslation \_ -> true
    pure mempty
  useEffect visible do
    launchAff_ $ runAnimation visible fade
    pure mempty
  pure $ M.getJsx
    $ container fade window.height props.wordPlacement do
        scrollView { style: M.css { height: 200, padding: 20 } } do
          fromMaybe mempty $ (append translationMarker) <$> translationText
          if props.sentence == props.phrase then mempty else sentenceSection phraseTranslation setPhraseTranslation props.phrase mutationFn props.language showPhraseTranslation setShowPhraseTranslation "Phrase"
          sentenceSection sentenceTranslation setSentenceTranslation props.sentence mutationFn props.language showSentenceTranslation setShowSentenceTranslation "Sentence"
          maybeDataMap props.morphology
  where
  visible = isJust props.translation || isJust props.morphology

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
  foldFn accum key value =
    accum
      <> M.view { style: M.css { flex: 1, alignSelf: "stretch", flexDirection: "row" } } do
          M.view { style: M.css { flex: 1, alignSelf: "stretch" } } $ M.text {} $ M.string key
          M.view { style: M.css { flex: 1, alignSelf: "stretch" } } $ M.text {} $ M.string $ mapValue key value
