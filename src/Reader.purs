module Reader where

import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Tuple (Tuple)
import React.Basic.Native as RN
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Type.Proxy (Proxy(..))
import EpubRn (epub, createStreamer, startStream, streamGet, killStream)
import Effect.Uncurried (mkEffectFn1)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..), try)
import Data.Either (Either(..))
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Markup as M
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Data.Traversable (traverse_)
import ApolloHooks (useMutation, gql)
import EpubUtil (mkStateChangeListeners, bridgeFile, HighlightedContent, epubjs)
import Data.Symbol (SProxy(..))
import Record (merge)
import Record.Builder (build, insert, modify, Builder)
import AsyncStorage (getItem, setItem)
import Data.Tuple (fst)
import BottomContent as BottomContent
import Foreign.Object (Object)
import Debug.Trace (spy)
import Wiktionary (getDefinition)

type VisibleLocation
  = { start :: { percentage :: Int, cfi :: String } }

colors = { noun: { color: "orange" }, adjective: { color: "red" }, verb: { color: "green" }, none: { color: "black" } }

type Props
  = { location :: String
    , height :: Number
    , width :: Number
    , toggleBars :: Effect Unit
    , setToc :: (Array String -> Array String) -> Effect Unit
    , setTitle :: (String -> String) -> Effect Unit
    , title :: String
    , setSliderDisabled :: (Boolean -> Boolean) -> Effect Unit
    , setVisibleLocation :: (VisibleLocation -> VisibleLocation) -> Effect Unit
    , visibleLocation :: VisibleLocation
    , showBars :: Boolean
    , setShowBars :: (Boolean -> Boolean) -> Effect Unit
    , setHeight :: (Number -> Number) -> Effect Unit
    , setWidth :: (Number -> Number) -> Effect Unit
    , route :: { params :: { slug :: String } }
    }

styles =
  { reader:
      { flex: 1
      , alignSelf: "stretch"
      , backgroundColor: "#3F3F3C"
      }
  , wrapper:
      { flex: 1
      , marginTop: 40
      , marginBottom: 40
      , zIndex: 1
      }
  }

type Query
  = { book :: { epubUrl :: Nullable String, processedEpubUrl :: Nullable String } }

query =
  gql
    """
  query routes_Book_Query($book: String) {
    currentUser(book: $book) {
      firstName
      lastName
      email
    }
    comments(book: $book, cfirange: "a") {
      text
      cfirange
    }
    book(slug: $book) {
      epubUrl
      processedEpubUrl
    }
  }
"""

locationChange title setVisibleLocation = mkEffectFn1 e
  where
  e :: VisibleLocation -> Effect Unit
  e event =
    launchAff_ do
      setItem title event.start.cfi
      liftEffect $ setVisibleLocation \_ -> event

locationsReady setSliderDisabled = mkEffectFn1 e
  where
  e :: String -> Effect Unit
  e locations = setSliderDisabled \_ -> false

error = mkEffectFn1 e
  where
  e :: String -> Effect Unit
  e message = log $ "EPUBJS-Webview " <> message

press toggleBars { highlightedContent: _ /\ setHighlightedContent, morphology: _ /\ setMorphology, translation: _ /\ setTranslation } = mkEffectFn1 e
  where
  e :: {} -> Effect Unit
  e book = do
    setHighlightedContent \_ -> Nothing
    setMorphology \_ -> Nothing
    setTranslation \_ -> Nothing
    toggleBars

ready setTitle setToc = mkEffectFn1 e
  where
  e ::
    { package ::
        { metadata :: { title :: String } }
    , navigation :: { toc :: Array String }
    } ->
    Effect Unit
  e book = do
    setTitle \_ -> book.package.metadata.title
    setToc \_ -> book.navigation.toc

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Reader" $ buildJsx

newtype UseStreamer h
  = UseStreamer (UseEffect (Maybe String) (UseState String (UseState String (UseData Query h))))

derive instance ntUseStreamer :: Newtype (UseStreamer h) _

useStreamer :: (Effect Unit) -> String -> Hook UseStreamer (Maybe { src :: String, origin :: String, url :: String })
useStreamer toggleBars book =
  coerceHook
    $ React.do
        result <- useData (Proxy :: Proxy Query) query { variables: { book: book }, errorPolicy: "all" }
        src /\ setSrc <- useState ""
        origin /\ setOrigin <- useState ""
        let
          streamer = createStreamer
        let
          maybeUrl = bookUrl <$> result
        let
          affFn = \url -> launchAff_ $ (streamerAff toggleBars streamer setOrigin setSrc url)
        useEffect maybeUrl
          $ do
              traverse_ affFn maybeUrl
              pure $ killStream streamer
        --pure $ streamerResult (Just {book: {epubUrl: toNullable $ Just $ "https://s3.amazonaws.com/epubjs/books/moby-dick.epub", processedEpubUrl: null}}) src origin
        pure $ streamerResult result src origin
  where
  streamerResult d src origin = (bookUrl >>> streamerRecord src origin) <$> d

  streamerRecord = { src: _, origin: _, url: _ }

  bookUrl = _.book >>> findUrl

  findUrl book = fromMaybe "" (maybeUrl book)

  maybeUrl book = (toMaybe book.processedEpubUrl) <|> (toMaybe book.epubUrl)

  streamerAff toggleBars streamer setOrigin setSrc url = do
    let
      delayAndToggle = do
        delay $ Milliseconds 1000.0
        liftEffect $ toggleBars
    origin <- (startStream streamer)
    fiber <- forkAff $ delayAndToggle
    liftEffect $ setOrigin $ \_ -> origin
    src <- streamGet streamer url
    liftEffect $ setSrc $ \_ -> src

mutation =
  gql
    """
  mutation translateMutation($input: TranslateInput!) {
    translate(input: $input) {
      translation
    }
  }
"""

useRenditionData showBars setShowBars visibleLocation = React.do
  mutationFn /\ result <- useMutation mutation {}
  translation /\ setTranslation <- useState $ (Nothing :: Maybe String)
  highlightedContent /\ setHighlightedContent <- useState $ (Nothing :: Maybe HighlightedContent)
  wiktionaryEntry /\ setWiktionaryEntry <- useState $ (Nothing :: Maybe String)
  sentence <- useState $ (Nothing :: Maybe String)
  epubcfi <- useState $ (Nothing :: Maybe String)
  morphology /\ setMorphology <- useState $ (Nothing :: Maybe (Object String))
  language /\ setLanguage <- useState $ (Nothing :: Maybe String)
  chapterTitle <- useState $ (Nothing :: Maybe String)
  useEffect highlightedContent
    $ do
        launchAff_ $ mutateAndChangeState mutationFn highlightedContent language setShowBars setTranslation
        pure mempty
  pure
    $ { translation: translation /\ setTranslation
      , highlightedContent: highlightedContent /\ setHighlightedContent
      , epubcfi
      , wiktionaryEntry: wiktionaryEntry /\ setWiktionaryEntry
      , morphology: morphology /\ setMorphology
      , language: language /\ setLanguage
      , sentence
      , chapterTitle
      }
  where
  mutateAndChangeState mutationFn (Just highlightedContent) (Just language) setShowBars setTranslation = do
    let
      payload = { variables: { input: { snippet: highlightedContent.text, language: language } } }
    _ <- spy "WIKTIONARY" $ getDefinition highlightedContent.text language
    result <- try $ mutationFn payload
    case result of
      Left err -> pure unit
      Right r -> do
        liftEffect $ setShowBars \_ -> false
        liftEffect $ setTranslation \_ -> Just r.translate.translation

  mutateAndChangeState _ _ _ _ setTranslation = do
    liftEffect $ setTranslation \_ -> Nothing

layoutEvent setHeight setWidth = mkEffectFn1 e
  where
  e :: RN.LayoutChangeEvent -> Effect Unit
  e event = do
    let
      { x, y, width, height } = (spy "event" event).nativeEvent.layout
    _ <- setHeight \_ -> height
    _ <- setWidth \_ -> width
    pure unit

getPosStates = do
  verb <- getPosState "verb"
  noun <- getPosState "noun"
  adjective <- getPosState "adjective"
  pure $ { verb, noun, adjective }
  where
  getPosState :: String -> Aff Boolean
  getPosState pos = do
    maybeValue <- (getItem $ posStorageKey pos)
    let
      valueWithDefault = fromMaybe "true" maybeValue
    pure $ valueWithDefault == "true"

posStorageKey pos = ("highlight-" <> pos)

togglePos :: String -> Tuple Boolean ((Boolean -> Boolean) -> Effect Unit) -> Effect Unit
togglePos pos (value /\ setter) =
  launchAff_
    $ do
        setItem (posStorageKey pos) $ show $ not value
        liftEffect $ setter \h -> not h

type Theme
  = { "[data-pos=\"VERB\"]" :: { color :: String }
    , "[data-pos=\"AUX\"]" :: { color :: String }
    , "[data-pos=\"NOUN\"]" :: { color :: String }
    , "[data-pos=\"ADJ\"]" :: { color :: String }
    }

defaultTheme =
  { p:
      { "line-height": 1.5
      }
  , body:
      { "font-family": "'Libre Baskerville', serif"
      , "-webkit-touch-callout": "none"
      , "-webkit-user-select": "none"
      , "-khtml-user-select": "none"
      , "-moz-user-select": "none"
      , "-ms-user-select": "none"
      , "user-select": "none"
      }
  }

setTheme :: Boolean -> Boolean -> Boolean -> Theme
setTheme highlightVerbs highlightNouns highlightAdjectives = build (adjectives $ nouns $ verbs $ defaultTheme) {}
  where
  verbs = setVerbs highlightVerbs

  nouns = setNouns highlightNouns

  adjectives = setAdjectives highlightAdjectives

  verbKey = SProxy :: SProxy "[data-pos=\"VERB\"]"

  auxKey = SProxy :: SProxy "[data-pos=\"AUX\"]"

  nounKey = SProxy :: SProxy "[data-pos=\"NOUN\"]"

  adjKey = SProxy :: SProxy "[data-pos=\"ADJ\"]"

  defaultTheme :: Builder {} Theme
  defaultTheme =
    insert verbKey colors.verb
      >>> insert auxKey colors.verb
      >>> insert nounKey colors.noun
      >>> insert adjKey colors.adjective

  setVerbs true theme = theme

  setVerbs false theme =
    defaultTheme
      >>> modify verbKey (\_ -> colors.none)
      >>> modify auxKey (\_ -> colors.none)

  setNouns true theme = theme

  setNouns false theme =
    defaultTheme
      >>> modify nounKey (\_ -> colors.none)

  setAdjectives true theme = theme

  setAdjectives false theme =
    defaultTheme
      >>> modify adjKey (\_ -> colors.none)

buildJsx props = React.do
  flow /\ setFlow <- useState "paginated"
  highlightVerbs /\ setHighlightVerbs <- useState $ true
  highlightNouns /\ setHighlightNouns <- useState $ true
  highlightAdjectives /\ setHighlightAdjectives <- useState $ true
  useEffect unit
    $ do
        launchAff_ do
          { verb, noun, adjective } <- getPosStates
          liftEffect $ setHighlightVerbs \_ -> verb
          liftEffect $ setHighlightNouns \_ -> noun
          liftEffect $ setHighlightAdjectives \_ -> adjective
        pure mempty
  streamResult <- useStreamer props.toggleBars props.route.params.slug
  stateChangeListeners <- useRenditionData props.showBars props.setShowBars props.visibleLocation
  useEffect (fst stateChangeListeners.highlightedContent)
    $ do
        log $ "listeners: " <> (fromMaybe "Nothing" ((_.text) <$> fst stateChangeListeners.highlightedContent))
        pure mempty
  case streamResult of
    Nothing -> pure mempty
    Just { src, origin } ->
      pure $ M.getJsx
        $ do
            M.view
              { style: M.css styles.wrapper
              , onLayout: layoutEvent props.setHeight props.setWidth
              } do
              M.childElement epub
                { style: M.css styles.reader
                , height: props.height
                , width: props.width
                , stateChangeListeners: mkStateChangeListeners stateChangeListeners
                , customHtml: bridgeFile
                , epubjs: epubjs
                , src: src
                , flow: flow
                , location: props.location
                , onLocationChange: locationChange props.title props.setVisibleLocation
                , onLocationsReady: locationsReady props.setSliderDisabled
                , onReady: ready props.setTitle props.setToc
                , themes: { highlighted: merge (setTheme highlightVerbs highlightNouns highlightAdjectives) defaultTheme }
                , theme: "highlighted"
                , onPress: press props.toggleBars stateChangeListeners
                , fontSize: "18px"
                , origin: origin
                , onError: error
                }
            M.childElement BottomContent.reactComponent
              { translation: (fst stateChangeListeners.translation), morphology: (fst stateChangeListeners.morphology), wordPlacement: _.fromTop <$> (fst stateChangeListeners.highlightedContent), sentence: (fst stateChangeListeners.sentence), language: (fst stateChangeListeners.language) }
