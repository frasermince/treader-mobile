module Reader where

import Prelude
import Context (dataStateContext)
import ComponentTypes (BookViewQuery)
import Effect (Effect)
import Effect.Class (liftEffect)
import Data.Tuple (Tuple)
import React.Basic.Native as RN
import React.Basic.Hooks as React
import Effect.Exception (message)
import Type.Proxy (Proxy(..))
import EpubRn (epub, createStreamer, startStream, streamGet, killStream, CFI, compare, toCfi)
import Effect.Uncurried (mkEffectFn4, EffectFn4)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..), try)
import Data.Either (Either(..))
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Markup as M
import Control.Alt ((<|>))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Newtype (class Newtype)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)
import Effect.Console (log)
import Effect.Unsafe (unsafePerformEffect)
import Data.Traversable (traverse_, traverse)
import ApolloHooks (useMutation, gql)
import EpubUtil (mkStateChangeListeners, bridgeFile, HighlightedContent, epubjs)
import Data.Symbol (SProxy(..))
import Record (merge)
import Record.Builder (build, insert, modify, Builder)
import AsyncStorage (getItem, setItem)
import Data.Tuple (fst, snd)
import BottomContent as BottomContent
import Foreign.Object (Object)
import Debug.Trace (spy)
import AppState (useAppState)
import Navigation (useFocusEffect)
import Subscribe as Subscribe
import Paper (portal)
import React.Basic.Native.Events as RNE
import ComponentTypes
import Data.DateTime (DateTime, diff)
import Data.Time.Duration (fromDuration)
import Effect.Now (nowDateTime)
import QueryHooks (useData, UseData, stripGraphqlError)
import Effect.Aff.Retry (recovering, constantDelay, limitRetries, RetryStatus(..))
import Segment (track, screen)

type LastAdvanced = {time :: DateTime, cfi :: CFI}
type AudioInformation = {startPageTime :: String, endPageTime :: String, index :: Int}
type VisibleLocation
  = { start :: { percentage :: Int, cfi :: String } }

colors = { noun: { color: "orange" }, adjective: { color: "red" }, verb: { color: "green" }, none: { color: "black" } }

type Props
  = { location :: String
    , height :: Number
    , width :: Number
    , toggleBars :: Effect Unit
    , setToc :: (Array String -> Array String) -> Effect Unit
    , setTitle :: (Maybe String -> Maybe String) -> Effect Unit
    , bookData :: Maybe BookViewQuery
    , title :: Maybe String
    , setSliderDisabled :: (Boolean -> Boolean) -> Effect Unit
    , setLocation :: (String -> String) -> Effect Unit
    , setVisibleLocation :: (VisibleLocation -> VisibleLocation) -> Effect Unit
    , visibleLocation :: VisibleLocation
    , showBars :: Boolean
    , setShowBars :: (Boolean -> Boolean) -> Effect Unit
    , setHeight :: (Number -> Number) -> Effect Unit
    , setWidth :: (Number -> Number) -> Effect Unit
    , slug :: String
    }

styles =
  { reader:
    { flex: 1
    , alignSelf: "stretch"
    , backgroundColor: "#3F3F3C"
    , marginTop: 20
    , marginBottom: 20
    }
  , wrapper:
    { flex: 1
    , marginTop: 40
    , zIndex: 1
    }
  }

locationChange
  :: Maybe String
  -> ((Maybe AudioInformation -> Maybe AudioInformation) -> Effect Unit)
  -> ((VisibleLocation -> VisibleLocation) -> Effect Unit)
  -> (Tuple (Maybe LastAdvanced) ((Maybe LastAdvanced -> Maybe LastAdvanced) -> Effect Unit))
  -> ((Int -> Int) -> Effect Unit)
  -> EffectFn4 VisibleLocation (Nullable String) (Nullable String) (Nullable Int) Unit
locationChange title setAudioInformation setVisibleLocation (pageLastAdvanced /\ setPageLastAdvanced) setPagesRead = mkEffectFn4 e
  where
  shouldIncrement :: LastAdvanced -> DateTime -> CFI -> Boolean
  shouldIncrement lastAdvanced now cfi = isAdvancing && isThirtySecondsApart
    where isThirtySecondsApart = (spy "APART" $ diff now lastAdvanced.time :: Milliseconds) > Milliseconds 20000.0
          isAdvancing = spy "IS GREATER" $ (compare cfi lastAdvanced.cfi) == GT

  incrementPages :: Maybe LastAdvanced -> DateTime -> CFI -> Aff Unit
  incrementPages Nothing now cfi =
    liftEffect $ setPageLastAdvanced \_ -> Just $ {time: spy "ADVANCED" now, cfi: cfi}

  incrementPages (Just lastAdvanced) now cfi
    | shouldIncrement lastAdvanced now cfi = do
        liftEffect $ setPagesRead \p -> p + 1
        liftEffect $ setPageLastAdvanced \_ -> Just $ {time: spy "ADVANCED" now, cfi: cfi}
    | otherwise = liftEffect $ setPageLastAdvanced \_ -> Just $ lastAdvanced {cfi = cfi}
  e :: VisibleLocation -> Nullable String -> Nullable String -> Nullable Int -> Effect Unit
  e event startPageTime endPageTime audioIndex =
    launchAff_ do
      now <- liftEffect nowDateTime
      incrementPages pageLastAdvanced now (toCfi (spy "EVENT" event).start.cfi)
      traverse_ (\t -> setItem t event.start.cfi) title
      liftEffect $ setAudioInformation  \_ -> do
        start <- toMaybe $ startPageTime
        end <- toMaybe $ endPageTime
        index <- toMaybe $ audioIndex
        pure $ {startPageTime: start, endPageTime: end, index: index}
      liftEffect $ log $ "***START" <> (show $ toMaybe $ startPageTime)
      liftEffect $ log $ "***END" <> (show $ toMaybe $ endPageTime)
      liftEffect $ log $ "***Index" <> (show $ toMaybe $ audioIndex)
      liftEffect $ setVisibleLocation \_ -> event

locationsReady setSliderDisabled = mkEffectFn1 e
  where
  e :: String -> Effect Unit
  e locations = setSliderDisabled \_ -> false

error = mkEffectFn1 e
  where
  e :: String -> Effect Unit
  e message = log $ "EPUBJS-Webview " <> message

press toggleBars { highlightedContent: highlightedContent /\ setHighlightedContent, selected: selected /\ setSelected} = mkEffectFn1 e
  where
  e :: {} -> Effect Unit
  e book = do
    if isNothing highlightedContent && not selected then toggleBars else setSelected \_ -> false

ready setTitle setToc setLocation = mkEffectFn1 e
  where
  e ::
    { package ::
      { metadata :: { title :: String } }
    , navigation :: { toc :: Array String }
    } ->
    Effect Unit
  e book = do
    setTitle \_ -> Just book.package.metadata.title
    setToc \_ -> book.navigation.toc
    launchAff_ do
      l <- getItem book.package.metadata.title
      liftEffect $ setLocation \_ -> fromMaybe "0" l


reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Reader" $ buildJsx

newtype UseStreamer h
  = UseStreamer (UseEffect (Maybe String) (UseState String (UseState String h)))

derive instance ntUseStreamer :: Newtype (UseStreamer h) _

useStreamer :: Maybe BookViewQuery -> ((Boolean -> Boolean) -> Effect Unit) -> (Effect Unit) -> Hook UseStreamer (Maybe { src :: String, origin :: String, url :: String, bookId :: String })
useStreamer bookData setLoaded toggleBars =
  coerceHook
    $ React.do
        src /\ setSrc <- useState ""
        origin /\ setOrigin <- useState ""
        let
          streamer = createStreamer
        let
          maybeUrl = bookUrl <$> bookData
        let
          affFn = \url -> launchAff_ $ (streamerAff toggleBars streamer setOrigin setSrc url)
        useEffect maybeUrl
          $ do
              traverse_ affFn maybeUrl
              pure $ killStream streamer
        --pure $ streamerResult (Just {book: {epubUrl: toNullable $ Just $ "https://s3.amazonaws.com/epubjs/books/moby-dick.epub", processedEpubUrl: null}}) src origin
        pure $ streamerResult bookData src origin
  where
  streamerResult d src origin = do
     bookResult <- d
     pure $ streamerRecord src origin (bookUrl bookResult) bookResult.book.id

  streamerRecord = { src: _, origin: _, url: _, bookId: _}

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

pageMutation = gql """
mutation pageCountMutation($input: UpdatePageCountInput!) {
  updatePageCount(input: $input) {
    result
  }
}
"""

mutation =
  gql
    """
  mutation translateMutation($input: TranslateInput!) {
    translateWithContext(input: $input) {
      translation
      is_permitted
    }
  }
"""

dailySelectionQuery = gql """
  query getSelections {
    dailySelections {
      id
    }
  }
"""


useRenditionData showBars setShowBars visibleLocation bookId addToPages setError = React.do
  mutationFn /\ result <- useMutation mutation {refetchQueries: [{query: dailySelectionQuery}]}
  translation /\ setTranslation <- useState $ (Nothing :: Maybe Translation)
  highlightedContent /\ setHighlightedContent <- useState $ (Nothing :: Maybe HighlightedContent)
  selected /\ setSelected <- useState false
  context /\ setContext <- useState $ (Nothing :: Maybe Context)
  epubcfi <- useState $ (Nothing :: Maybe String)
  morphology /\ setMorphology <- useState $ (Nothing :: Maybe (Object String))
  language /\ setLanguage <- useState $ (Nothing :: Maybe String)
  chapterTitle <- useState $ (Nothing :: Maybe String)
  ref <- useRef null

  useAppState
    $ { onForeground:
        do
          result <- readRefMaybe ref
          traverse_ (\s -> s.clearSelected) result
          setHighlightedContent \_ -> Nothing
          setShowBars \_ -> false,
        onBackground: addToPages
      }
  useEffect context
    $ do
        launchAff_ do
           let payload = makePayload highlightedContent language context bookId
           liftEffect $ setTranslation \_ -> Nothing
           mutateAndChangeState mutationFn payload setShowBars setTranslation setSelected
        pure mempty
  pure
    $ ref
    /\ { translation: translation /\ setTranslation
      , highlightedContent: highlightedContent /\ setHighlightedContent
      , epubcfi
      , morphology: morphology /\ setMorphology
      , language: language /\ setLanguage
      , context: context /\ setContext
      , selected: selected /\ setSelected
      , chapterTitle
      }
  where
  makePayload (Just highlightedContent) (Just language) (Just context) (Just bookId) = Just {
    variables: {
      input: {
        snippet: highlightedContent.text,
        language: language,
        phrase: toNullable $ context.phrase,
        sentence: toNullable $ context.sentence,
        phraseOffset: toNullable $ context.phraseOffset,
        sentenceOffset: toNullable $ context.sentenceOffset,
        wordLength: context.wordLength,
        bookId: bookId
      }
    }
  }

  makePayload _ _ _ _ = Nothing
  mutateAndChangeState mutationFn (Just payload) setShowBars setTranslation setSelected = do
    liftEffect $ setSelected \_ -> true
    let mutation = mutationFn $ spy "PAYLOAD" payload
    let lastIteration = 5
    let retryPolicy = constantDelay (Milliseconds 200.0) <> limitRetries 5
    let checks = pure \(RetryStatus rs) -> const $ pure (rs.iterNumber /= lastIteration)
    let retriedMutation = recovering retryPolicy checks $ \_ -> mutation
    result <- try $ retriedMutation
    case result of
      Left err -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message err
      Right r -> do
        _ <- track "Select Word" {bookId: payload.variables.input.bookId}
        liftEffect $ setShowBars \_ -> false
        liftEffect $ setTranslation \_ -> Just {text: r.translateWithContext.translation, isPermitted: r.translateWithContext.is_permitted}

  mutateAndChangeState _ _ _ setTranslation _ = do
    liftEffect $ setTranslation \_ -> Nothing

layoutEvent setHeight setWidth = mkEffectFn1 e
  where
  e :: RN.LayoutChangeEvent -> Effect Unit
  e event = do
    let
      { x, y, width, height } = (spy "event" event).nativeEvent.layout
    _ <- setHeight \_ -> height - 20.0
    _ <- setWidth \_ -> width - 20.0
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
    { "line-height": 1.8
    }
  , body:
    { "font-family": "'Roboto', sans-serif"
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
    theme
      >>> modify verbKey (\_ -> colors.none)
      >>> modify auxKey (\_ -> colors.none)

  setNouns true theme = theme

  setNouns false theme =
    theme
      >>> modify nounKey (\_ -> colors.none)

  setAdjectives true theme = theme

  setAdjectives false theme =
    theme
      >>> modify adjKey (\_ -> colors.none)

getBookId result = do
  r <- result
  pure $ r.bookId
buildJsx props = React.do
  audioInformation /\ setAudioInformation <- useState (Nothing :: Maybe AudioInformation)
  { setLoading, setError } <- useContext dataStateContext
  loaded /\ setLoaded <- useState false
  flow /\ setFlow <- useState "paginated"
  pageLastAdvanced <- useState (Nothing :: Maybe LastAdvanced)
  pagesRead /\ setPagesRead <- useState 0
  highlightVerbs /\ setHighlightVerbs <- useState $ true
  highlightNouns /\ setHighlightNouns <- useState $ true
  highlightAdjectives /\ setHighlightAdjectives <- useState $ true
  mutationFn /\ d <- useMutation pageMutation { errorPolicy: "all" }
  modalVisible /\ setModalVisible <- useState false

  useEffect unit
    $ do
        launchAff_ do
          _ <- screen "Book" {bookSlug: props.slug}
          { verb, noun, adjective } <- getPosStates
          liftEffect $ setHighlightVerbs \_ -> verb
          liftEffect $ setHighlightNouns \_ -> noun
          liftEffect $ setHighlightAdjectives \_ -> adjective
        pure mempty

  streamResult <- useStreamer props.bookData setLoaded props.toggleBars

  let addToPages = launchAff_ do
        liftEffect $ log $ "PAGES READ: " <> show pagesRead
        let mutate bookId
              | pagesRead > 0 = do
                  result <- try $ (spy "MUTATION FN" $ mutationFn) {variables: {input: {bookId: bookId, pageCount: pagesRead}}}
                  case result of
                    Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
                    Right resp -> do
                      liftEffect $ setPagesRead \_ -> 0
              | otherwise = mempty
        traverse_ mutate $ getBookId streamResult

  useFocusEffect pagesRead do
     pure $ unit
     pure $ addToPages

  ref /\ stateChangeListeners <- useRenditionData props.showBars props.setShowBars props.visibleLocation (getBookId streamResult) addToPages setError
  useEffect props.slug do
     setLoaded \_ -> false
     snd stateChangeListeners.highlightedContent $ \_ -> Nothing
     props.setVisibleLocation \_ -> { start: { percentage: 0, cfi: "0" } }
     props.setTitle \_ -> Nothing
     pure mempty

  useEffect (fst stateChangeListeners.highlightedContent)
    $ do
        log $ "listeners: " <> (fromMaybe "Nothing" ((_.text) <$> fst stateChangeListeners.highlightedContent))
        pure mempty
  case streamResult of
    Nothing -> pure mempty
    Just { src, origin } ->
      pure $ M.getJsx
        $ do
            portal {} $ M.childElement Subscribe.reactComponent {visible: modalVisible, onDismiss: setModalVisible \_ -> false}
            M.view
              { style: M.css styles.wrapper
              , onLayout: layoutEvent props.setHeight props.setWidth
              } do
              M.childElement epub
                { ref: ref
                , style: M.css styles.reader
                , height: props.height
                , width: props.width
                , stateChangeListeners: mkStateChangeListeners stateChangeListeners
                , customHtml: bridgeFile
                , epubjs: epubjs
                , src: src
                , flow: flow
                , location: props.location
                , onLocationChange: locationChange props.title setAudioInformation props.setVisibleLocation pageLastAdvanced setPagesRead
                , onLocationsReady: locationsReady props.setSliderDisabled
                , onReady: ready props.setTitle props.setToc props.setLocation
                , themes: { highlighted: merge (setTheme highlightVerbs highlightNouns highlightAdjectives) defaultTheme }
                , theme: "highlighted"
                , onPress: press props.toggleBars stateChangeListeners
                , loaded: loaded
                , setLoaded: mkEffectFn1 $ \x -> setLoaded \_ -> x
                , fontSize: "20px"
                , origin: origin
                , onError: error
                }

            M.childElement BottomContent.tabs
              { translation: (fst stateChangeListeners.translation)
              , morphology: (fst stateChangeListeners.morphology)
              , wordPlacement: _.fromTop <$> (fst stateChangeListeners.highlightedContent)
              , removeContent: (snd stateChangeListeners.highlightedContent $ \_ -> Nothing)
              , context: (fst stateChangeListeners.context)
              , language: (fst stateChangeListeners.language)
              , setMorphology: (snd stateChangeListeners.morphology)
              , setTranslation: (snd stateChangeListeners.translation)
              , setModalVisible: setModalVisible
              , highlightVerbs
              , highlightNouns
              , highlightAdjectives
              , setHighlightVerbs
              , setHighlightNouns
              , setHighlightAdjectives
              , word: _.text <$> (fst stateChangeListeners.highlightedContent)
              }
