module Reader where
import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Tuple (Tuple)
import ApolloHooks (gql)
import Debug.Trace (spy)
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Type.Proxy (Proxy(..))
import EpubRn (epub, createStreamer, startStream, streamGet, killStream)
import Effect.Uncurried (mkEffectFn1)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..))
import Data.Nullable (Nullable, toMaybe)
import Markup as M
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Newtype (class Newtype)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Data.Traversable (traverse_)
import ApolloHooks (useMutation)
import EpubUtil (mkStateChangeListeners, bridgeFile)
import Data.Symbol (SProxy(..))
import Record.Builder (build, insert, modify, Builder)
import AsyncStorage (getItem, setItem)

type VisibleLocation = {start :: {percentage :: Int}}

colors = {noun: {color: "orange"}, adjective: {color: "red"}, verb: {color: "green"}, none: {color: "black"}}

type Props = {
  location :: String,
  height :: Number,
  width :: Number,
  toggleBars :: Effect Unit,
  setToc :: (Array String -> Array String) -> Effect Unit,
  setTitle :: (String -> String) -> Effect Unit,
  setSliderDisabled :: (Boolean -> Boolean) -> Effect Unit,
  setVisibleLocation :: (VisibleLocation -> VisibleLocation) -> Effect Unit,
  navigation :: {
    navigate :: String -> Effect Unit,
    state :: {params :: {slug :: String}}
  }
}

styles = {
  reader:
    { flex: 1
    , alignSelf: "stretch"
    , backgroundColor: "#3F3F3C"
    }
}

type Query = {book :: {epubUrl :: Nullable String, processedEpubUrl :: Nullable String}}

query = gql """
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

locationChange setVisibleLocation = mkEffectFn1 e
  where
  e :: {start :: {percentage :: Int}} -> Effect Unit
  e event = setVisibleLocation \_ -> event

locationsReady setSliderDisabled = mkEffectFn1 e
  where
  e :: String -> Effect Unit
  e locations = setSliderDisabled \_ -> false


error = mkEffectFn1 e
  where
  e :: String -> Effect Unit
  e message = log $ "EPUBJS-Webview " <> message

press toggleBars = mkEffectFn1 e
  where
  e :: {} -> Effect Unit
  e book = toggleBars

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
reactComponent = unsafePerformEffect $ do
  component "Reader" $ buildJsx

newtype UseStreamer h = UseStreamer (UseEffect (Maybe String) (UseState String (UseState String (UseData Query h))))
derive instance ntUseStreamer :: Newtype (UseStreamer h) _

useStreamer :: (Effect Unit) -> String -> Hook UseStreamer (Maybe {src :: String, origin :: String, url :: String})
useStreamer toggleBars book = coerceHook $ React.do
  result <- useData (Proxy :: Proxy Query) query {variables: {book: book}}
  src /\ setSrc <- useState ""
  origin /\ setOrigin <- useState ""
  let streamer = createStreamer
  let maybeUrl = bookUrl <$> result
  let affFn = \url -> launchAff_ $ (streamerAff toggleBars streamer setOrigin setSrc url)

  useEffect maybeUrl $ do
    traverse_ affFn maybeUrl
    pure $ killStream streamer
  pure $ streamerResult result src origin

  where streamerResult d src origin = (bookUrl >>> streamerRecord src origin) <$> d
        streamerRecord = {src: _, origin: _, url: _}
        bookUrl = _.book >>> findUrl
        findUrl book = fromMaybe "" (maybeUrl book)
        maybeUrl book = (toMaybe book.processedEpubUrl) <|> (toMaybe book.epubUrl)
        streamerAff toggleBars streamer setOrigin setSrc url = do
          let delayAndToggle = do
                delay $ Milliseconds 1000.0
                liftEffect $ toggleBars
          fiber <- forkAff $ delayAndToggle
          origin <- (startStream streamer)
          liftEffect $ setOrigin $ \_ -> origin
          src <- streamGet streamer url
          liftEffect $ setSrc $ \_ -> src

mutation = gql """
  mutation translateMutation($input: TranslateInput!) {
    translate(input: $input) {
      translation
    }
  }
"""

useRenditionData = React.do
  mutationFn /\ result <- useMutation mutation {}
  translation <- useState $ (Nothing :: Maybe String)
  highlightedContent <- useState $ (Nothing :: Maybe String)
  epubcfi <- useState $ (Nothing :: Maybe String)
  morphology <- useState $ (Nothing :: Maybe {})
  language <- useState $ (Nothing :: Maybe String)
  chapterTitle <- useState $ (Nothing :: Maybe String)
  let mutateAndStateChange = \(value /\ setter) x -> launchAff_ do
        result <- mutationFn x
        liftEffect $ setter \_ -> result.data.translate.translation


  pure $
    { translation: translation
    , highlightedContent
    , epubcfi
    , morphology
    , language
    , chapterTitle
    } /\ { mutationFn: mutateAndStateChange translation}


getPosStates = do
  verb <- getPosState "verb"
  noun <- getPosState "noun"
  adjective <- getPosState "adjective"
  pure $ {verb, noun, adjective}
  where getPosState :: String -> Aff Boolean
        getPosState pos =  do
           maybeValue <- (getItem $ posStorageKey pos)
           let valueWithDefault = fromMaybe "true" maybeValue
           pure $ valueWithDefault == "true"

posStorageKey pos = ("highlight-" <> pos)
togglePos :: String -> Tuple Boolean ((Boolean -> Boolean) -> Effect Unit) -> Effect Unit
togglePos pos (value /\ setter) = launchAff_ $ do
    setItem (posStorageKey pos) $ show $ not value
    liftEffect $ setter \h -> not h

type Theme = {
  "[data-pos=\"VERB\"]" :: {color :: String},
  "[data-pos=\"AUX\"]" :: {color :: String},
  "[data-pos=\"NOUN\"]" :: {color :: String},
  "[data-pos=\"ADJ\"]" :: {color :: String}
}


setTheme :: Boolean -> Boolean -> Boolean -> Theme
setTheme highlightVerbs highlightNouns highlightAdjectives =
  build (adjectives $ nouns $ verbs $ defaultTheme) {}
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
            insert verbKey colors.verb >>>
            insert auxKey colors.verb >>>
            insert nounKey colors.noun >>>
            insert adjKey colors.adjective

        setVerbs true theme = theme
        setVerbs false theme =
            defaultTheme >>>
            modify verbKey (\_ -> colors.none) >>>
              modify auxKey (\_ -> colors.none)

        setNouns true theme = theme
        setNouns false theme =
            defaultTheme >>>
            modify nounKey (\_ -> colors.none)

        setAdjectives true theme = theme
        setAdjectives false theme =
            defaultTheme >>>
            modify adjKey (\_ -> colors.none)

buildJsx props = React.do
  flow /\ setFlow <- useState "paginated"
  highlightVerbs /\ setHighlightVerbs <- useState $ true
  highlightNouns /\ setHighlightNouns <- useState $ true
  highlightAdjectives /\ setHighlightAdjectives <- useState $ true
  useEffect unit $ do
     launchAff_ do
        {verb, noun, adjective} <- getPosStates
        liftEffect $ setHighlightVerbs \_ -> verb
        liftEffect $ setHighlightNouns \_ -> noun
        liftEffect $ setHighlightAdjectives \_ -> adjective
     pure mempty


  streamResult <- useStreamer props.toggleBars props.navigation.state.params.slug
  stateChangeListeners /\ eventFns <- useRenditionData
  case streamResult of
       Nothing -> pure mempty
       Just {src, origin} -> pure $ element
         epub
            { style: M.css styles.reader
            , height: props.height
            , width: props.width
            , stateChangeListeners: mkStateChangeListeners stateChangeListeners
            , eventFns: eventFns
            , bridge: bridgeFile
            , src: src
            , flow: flow
            , location: props.location
            , onLocationChange: locationChange props.setVisibleLocation
            , onLocationsReady: locationsReady props.setSliderDisabled
            , onReady: ready props.setTitle props.setToc
            , themes: {highlighted: setTheme highlightVerbs highlightNouns highlightAdjectives}
            , theme: "highlighted"
            , onPress: press props.toggleBars
            , origin: origin
            , onError: error
            }

