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
import Paper (surface, switch, title, toggleButton)
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
import Data.String (length, stripSuffix, stripPrefix, Pattern(..))
import Blur (blurView)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Navigation (useNavigation)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Data.Traversable (traverse_)
import ComponentTypes
import TabView (sceneMap, tabView, tabBar)
import Record.Unsafe.Union (unsafeUnion)
import Debug.Trace (spy)
import Wiktionary (getDefinition, WiktionaryResult)
import Effect.Console (log)
import HTMLView (htmlView)
import Data.Foldable (foldl)
import Data.Interpolate (i)

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

switchRowStyle = M.css {flex: 1, flexDirection: "row", alignItems: "center", justifyContent: "space-between", width: "85%"}
colorToggles props = React.do
  pure $ M.getJsx $ do
     M.view {style: M.css {height: "50%", marginLeft: "5%"}} do
      M.view {style: switchRowStyle } do
        M.text {style: M.css {marginLeft: 10}} $ M.string "Nouns"
        switch {style: M.css {}, value: props.highlightNouns, onValueChange: props.setHighlightNouns \n -> not n, color: "orange"}
      M.view {style: switchRowStyle } do
        M.text {style: M.css {marginLeft: 10}} $ M.string "Verbs"
        switch {style: M.css {}, value: props.highlightVerbs, onValueChange: props.setHighlightVerbs \n -> not n, color: "green"}
      M.view {style: switchRowStyle } do
        M.text {style: M.css {marginLeft: 10}} $ M.string "Adjectives"
        switch {style: M.css {}, value: props.highlightAdjectives, onValueChange: props.setHighlightAdjectives \n -> not n, color: "red"}

wordColors :: ReactComponent Props
wordColors = unsafePerformEffect
    $ do
        (component "WordColors") colorToggles

definitionJsx props = React.do
  term /\ setTerm <- useState (Nothing :: Maybe String)
  wiktionaryEntry /\ setWiktionaryEntry <- useState $ (Nothing :: Maybe String)
  useEffect props.word do
     setTerm \_ -> Nothing
     pure mempty
  useEffect (term /\ props.language /\ props.word) do
    launchAff_ do
      result <- wiktionary (term <|> props.word) props.language $ Just "en"
      liftEffect $ setWiktionaryEntry \_ -> Just $ dictHtml $ result
    pure mempty
  pure $ M.getJsx $ case wiktionaryEntry of
       Just d ->
         M.scrollView { style: M.css {marginLeft: 20, marginRight: 20, paddingTop: 20 } } do
          title {style: M.css {textAlign: "center"}} $ M.string $ fromMaybe "" $ term <|> props.word
          htmlView {value: d, onLinkPress: mkEffectFn1 \url -> (setTerm \_ -> wordFromUrl url), stylesheet: {h4: {textAlign: "center"}}}
       Nothing -> pure mempty

  where wiktionary (Just text) (Just language) (Just locale) = spy "HERE" getDefinition text locale language
        wiktionary _ _ _ = pure []
        wordFromUrl url = do
           prefixless <- stripPrefix (Pattern "/wiki/") $ spy "URL" url
           stripSuffix (Pattern "#French") prefixless
        dictHtml :: WiktionaryResult -> String
        dictHtml d = foldl foldDefinitions "" d
        foldDefinitions accum d = i accum "<h4>" d.partOfSpeech "</h4>" "<ol>" (foldl foldDefinition "" d.definitions) "</ol>"
        foldDefinition accum d = i accum "<li>" d.definition "</li>"

definition :: ReactComponent Props
definition = unsafePerformEffect
    $ do
        (component "Definition") definitionJsx


tabs :: ReactComponent Props
tabs = unsafePerformEffect
    $ do
        (component "BottomTabs") buildTabs


buildTabs props = React.do
  index /\ setIndex <- useState 0
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

  let renderTabBar = \props -> M.getJsx do
        tabBar $ unsafeUnion props {
            style: M.css {backgroundColor: "white", color: "black"},
            indicatorStyle: M.css {backgroundColor: "#66aab1" },
            activeColor: "black",
            inactiveColor: "black"
          }
  let routes = [{key: "wordInformation", title: "Main"}, {key: "definition", title: "Wiktionary"}, {key: "wordColors", title: "Color Key"}]
  let renderScene = sceneMap {wordInformation: reactComponent}
  let renderScene = \{route} ->
                    case route.key of
                         "wordInformation" -> element reactComponent props
                         "wordColors" -> element wordColors props
                         "definition" -> element definition props
                         otherwise -> element reactComponent props
  pure $ M.getJsx
    $ container fade window.height placementForAnimation do
       tabView {renderTabBar, style: M.css {backgroundColor: "white"}, navigationState: {index, routes}, renderScene, onIndexChange: mkEffectFn1 \i -> setIndex \_ -> i}
  where visible = isJust props.wordPlacement


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
    removeContent :: Effect Unit,
    setHighlightVerbs :: (Boolean -> Boolean) -> Effect Unit,
    setHighlightNouns :: (Boolean -> Boolean) -> Effect Unit,
    setHighlightAdjectives :: (Boolean -> Boolean) -> Effect Unit,
    highlightVerbs :: Boolean,
    highlightNouns :: Boolean,
    highlightAdjectives :: Boolean,
    word :: Maybe String
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
    height: 350
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
  , elevation: zIndex
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
  | (floor height) - (floor wordPlacement) < 550 = surface {style: M.css $ merge (styles fade) { top: 0}} $ M.view {style: M.css {flex: 1, marginTop: 30}} children
  | otherwise = surface {style: M.css $ merge (styles fade) { bottom: 0 }} children

container fade height wordPlacement children = surface {style: M.css $ merge (styles fade) { bottom: 0 }} children

buildJsx props = React.do
  navigation <- useNavigation
  mutationFn /\ result <- useMutation mutation {}
  ref <- useRef null
  useEffect props.wordPlacement do
     result <- readRefMaybe ref
     traverse_ (\s -> scrollTo (getNode s) 0) result
     pure mempty

  pure $ M.getJsx do
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
