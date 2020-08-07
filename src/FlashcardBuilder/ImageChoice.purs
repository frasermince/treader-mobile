module FlashcardBuilder.ImageChoice where

import Prelude
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon, subheading, caption)
import Effect (Effect)
import Record.Unsafe.Union (unsafeUnion)
import ImageSearch (imageSearch, Image)
import Subscribe as Subscribe
import ApolloHooks (useMutation, gql)
import Markup as M
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Effect.Unsafe (unsafePerformEffect)
import Image (image)
import Dimensions (window)
import Context (dataStateContext, Context)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, mkEffectFn2, runEffectFn2, EffectFn2)
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
import MaterialIcon (icon)
import Data.Map (fromFoldable, lookup)
import Data.Traversable (traverse_, traverse)
import Data.FoldableWithIndex (foldlWithIndexDefault)
import Data.Foldable(foldl)
import ComponentTypes (Selection)
import Data.String (stripPrefix, Pattern(..))
import Data.Array ((:))
import Ebisu as Ebisu
import KeyboardAwareDialog (keyboardAwareDialog)
import Affjax.ResponseFormat as ResponseFormat
import Data.HTTP.Method (Method(..))
import Affjax as AX
import Affjax.RequestBody as RequestBody
import Data.Argonaut.Core as J
import Config (config)
import Record.Unsafe (unsafeGet)
import Data.Maybe (Maybe(..), fromMaybe, isJust)
import FS (audioDir, exists, unlink, absintheFile)
import Effect.Console (log)
import Data.Argonaut.Encode (encodeJson)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either(..))
import Foreign.Object (lookup) as Object
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\), useLayoutEffect)
import Sound (play, Sound, release, stop, createSound, stopAndPlay)
import Blur (blurView)
import QueryHooks (useData, UseData, stripGraphqlError)
import Type.Proxy (Proxy(..))
import Data.Nullable (null)
import Segment (track)
import Debug.Trace (spy)
import WiktionaryModal as WiktionaryModal
import FetchAudio (useAudio, defaultAudioFile)
import Data.Interpolate (i)

data Payload = NewSentencePayload
  (String -> {variables :: {input :: { a :: Number
  , b :: Number
  , t :: Number
  , selectedSnippetId :: Int
  , startOffset :: Int
  , word :: String
  , imageUrl :: Array String
  , bookId :: Int
  , sentenceText :: String
  , language :: String
  , sentenceTranslation :: String
  , audio :: String
  }}}) | ExistingSentencePayload
  {variables :: {input :: { a :: Number
  , b :: Number
  , t :: Number
  , startOffset :: Int
  , word :: String
  , imageUrl :: Array String
  , bookId :: Int
  , sentenceText :: String
  }}}

type Props = {
  route ::
    {
      params ::
        {
          selection :: Selection,
          range :: String,
          wordTranslation :: String,
          rangeTranslation :: String,
          rangeOffset :: Int,
          word :: String,
          existingSentence :: Boolean,
          audio :: Maybe String
        }
    },
        navigation :: {
          push :: EffectFn2 String { sentenceId :: Int, selection :: Selection } Unit,
          navigate :: EffectFn2 String {word :: String} Unit,
          setOptions :: EffectFn1 {headerRight :: ReactComponent {}} Unit
        }
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

text :: EventFn (RNE.NativeSyntheticEvent String) String
text = unsafeEventFn \e -> (unsafeCoerce e)

translateIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "google-translate" }
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

selectedImages selectedIndices images =
  foldlWithIndexDefault foldFn [] images
  where foldFn index accum image = if isSelected selectedIndices index then image.thumbnailLink : accum else accum

isSelected selected index = fromMaybe false $ selected !! index
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
  M.touchableOpacity {onPress: RNE.capture_ $ determineSelection setSelected index} do
    if isSelected selected index then badge {style: M.css {position: "absolute", zIndex: 10, top: 2, right: 10, backgroundColor: "#66aab1" }} $ icon {color: "white", name: "check", size: 14} else mempty
    image {style: M.css $ imageStyle (isSelected selected index), source: {uri: i.item.thumbnailLink}}

makePayload :: Selection -> String -> String -> Int -> Array String -> String -> Boolean -> Payload
makePayload selection range rangeTranslation rangeOffset imageUrl word false = NewSentencePayload $ \audio ->
  { variables:
    { input:
      {
        a: a,
        b: b,
        t: t,
        selectedSnippetId: selection.id,
        startOffset: rangeOffset,
        word: word,
        imageUrl: imageUrl,
        bookId: selection.book.id,
        sentenceText: range,
        language: selection.book.language,
        audio: audio,
        sentenceTranslation: rangeTranslation
      }
    }
  }
  where a /\ b /\ t = Ebisu.defaultModel 24.0

makePayload selection range rangeTranslation rangeOffset imageUrl word true = ExistingSentencePayload
  { variables:
    { input:
      {
        a: a,
        b: b,
        t: t,
        startOffset: rangeOffset,
        word: word,
        imageUrl: imageUrl,
        bookId: selection.book.id,
        sentenceText: range
      }
    }
  }
  where a /\ b /\ t = Ebisu.defaultModel 24.0

saveFlashcard mutate mutateWithSentence (NewSentencePayload payload) fetch setError redirect text language setLoading setShouldBlur = launchAff_ do
  liftEffect $ runEffectFn1 setLoading $ \_ -> true
  maybeFetch <- fetch text language
  let path = fromMaybe defaultAudioFile do
        result <- maybeFetch
        pure $ result.path
  let completedPayload = payload $ absintheFile {uri: "file://" <> path, name: "speech.mp3", type: "application/mpeg"}
  result <- try $ mutateWithSentence $ completedPayload
  liftEffect $ runEffectFn1 setLoading $ \_ -> false
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
    Right resp -> case resp.createFlashcardWithSentence.isPermitted of
                       false -> liftEffect $ setShouldBlur \_ -> true
                       true -> do
                          _ <- track "Flashcard Created" {}
                          liftEffect $ redirect resp.createFlashcardWithSentence.flashcard.sentenceId

saveFlashcard mutate mutateWithSentence (ExistingSentencePayload payload) _ setError redirect _ _ setLoading setShouldBlur = launchAff_ do
  liftEffect $ runEffectFn1 setLoading $ \_ -> true
  result <- try $ mutate payload
  liftEffect $ runEffectFn1 setLoading $ \_ -> false
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
    Right resp -> case resp.createFlashcard.isPermitted of
                       false -> liftEffect $ setShouldBlur \_ -> true
                       true -> do
                          _ <- track "Flashcard Created" {}
                          liftEffect $ redirect resp.createFlashcard.flashcard.sentenceId


searchFromDialog setShowSearch setSelected search refetch setError language = do
  setShowSearch \_ -> false
  setSelected \_ -> replicate 8 false
  log $ "HERE: " <> search
  refetch {keyword: search, start: 0, num: 8, language: null}

type Query
  = { imageSearch :: Array {thumbnailLink :: String, contextLink :: String} }

imageQuery =
  gql """
  query imageQuery($keyword: String, $language: String, $start: Int, $num: Int) {
    imageSearch(keyword: $keyword, language: $language, start: $start, num: $num) {
      thumbnailLink
      contextLink
    }
  }
  """

flashcardMutation =
  gql
    """
mutation flashcardMutation($input: LoginInput!) {
  createFlashcard(input: $input) {
    flashcard {sentenceId}
    isPermitted
  }
}
  """

withSentenceMutation =
  gql
    """
mutation flashcardMutation($input: LoginInput!) {
  createFlashcardWithSentence(input: $input) {
    flashcard {sentenceId}
    isPermitted
  }
}
  """

noneSelected selected = foldl (\accum s -> accum && not s) true selected

paragraphItem params false = M.getJsx $ paragraph {} $ M.jsx $ [ underlineWord params.range params.rangeOffset params.word (M.css {fontWeight: "bold"}) "bold" 16]
paragraphItem params true = M.getJsx $ paragraph {} $ M.string $ params.rangeTranslation

unpermittedBlur setModalVisible = do
    blurView {style: M.css blurStyle, blurType: "light", blurAmount: 5}
    M.touchableOpacity {style: M.css blurTextStyle, onPress: blurPress} do
      M.text {style: M.css {}} $ M.string "You have reached your limit on creating flashcards for the day. Press to subscribe and create unlimited flashcards."
  where blurPress = RNE.capture_ do
          setModalVisible \_ -> true

buildJsx props = React.do
  { setLoading, setError } <- useContext dataStateContext
  mutateWithSentence /\ d1 <- useMutation withSentenceMutation { errorPolicy: "all" }
  mutate /\ d2 <- useMutation flashcardMutation { errorPolicy: "all" }
  let params = props.route.params
  let audioContext = do
        url <- params.audio
        pure $ {url, text: params.range}
  {play: playText, fetch} <- useAudio "image-choice" audioContext
  let navigate = runEffectFn2 props.navigation.navigate

  let selection = params.selection
  imagesResult <- useData (Proxy :: Proxy Query) imageQuery {variables: {keyword: params.word, start: 0, num: 8, language: "lang_" <> selection.book.language}, fetchPolicy: "cache-and-network"}
  let images result = fromMaybe [] do
        i <- result.state
        pure $ i.imageSearch
  shouldBlur /\ setShouldBlur <- useState false
  modalVisible /\ setModalVisible <- useState false
  showDefinition /\ setShowDefinition <- useState false
  selected /\ setSelected <- useState $ replicate 8 false
  search /\ setSearch <- useState params.word
  showSearch /\ setShowSearch <- useState false
  showTranslation /\ setShowTranslation <- useState false

  let payload = makePayload selection params.range params.rangeTranslation params.rangeOffset (selectedImages selected $ images imagesResult) params.word params.existingSentence
  pure $ M.getJsx do
    portal {} $ M.childElement Subscribe.reactComponent {visible: modalVisible, onDismiss: setModalVisible \_ -> false}
    portal {} $ M.childElement WiktionaryModal.reactComponent {word: params.word, visible: showDefinition, onDismiss: setShowDefinition \_ -> false, language: selection.book.language}
    portal {} do
      keyboardAwareDialog {visible: showSearch, onDismiss: setShowSearch \_ -> false} do
        dialogTitle {} $ M.string "Search Google For Images"
        dialogContent {} do
            textInput { placeholder: "Search", onChangeText: changeField setSearch, value: search }
        dialogActions {} do
            button {onPress: RNE.capture_ $ setShowSearch \_ -> false} $ M.string "Cancel"
            button {onPress: RNE.capture_ $ searchFromDialog setShowSearch setSelected search imagesResult.refetch setError selection.book.language} $ M.string "Search"

    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      surface { style: M.css { flex: 1 } } do
         M.view {style: M.css {alignItems: "center"}} do
           caption {} $ M.string "Build a visual, fill in the blank flashcard to help your memory"
         M.view {style: M.css {flex: 2, justifyContent: "flex-end", marginLeft: 15}} do
            M.view {style: M.css {flexDirection: "row"}} do
              M.view {style: M.css {flex: 1}} do
                headline {} $ M.string params.word
                M.text {style: M.css {marginBottom: 30}} $ M.string params.wordTranslation
              M.view {style: M.css {flex: 1}} do
                button {style: M.css {marginTop: 15, marginLeft: 10}, onPress: RNE.capture_ $ setShowDefinition \_ -> true} $ M.string "Get Definition"
         M.view {style: M.css {flex: 6}} do
           M.view {style: M.css {flex: 1}} do
              divider {style: M.css {height: 1, width: "100%"}}
              M.view {style: M.css {flex: 7, paddingTop: "5%", paddingLeft: 5, paddingRight: 5}} do
                fab {icon: "volume-medium", small: true, style: M.css {width: 40}, onPress: RNE.capture_ $ (spy "PLAY" playText) params.range $ spy "***LANGUAGE" selection.book.language}
                listItem {titleNumberOfLines: 5, onPress: RNE.capture_ $ setShowTranslation \t -> not t, title: paragraphItem params showTranslation, right: translateIcon, style: M.css {paddingTop: 10}}
              --paragraph {style: M.css {paddingTop: 20, paddingLeft: 5, paddingRight: 5}} $ M.string $ params.rangeTranslation
              subheading {style: M.css {marginLeft: 25, flex: 1}} $ M.string $ i "Searching for images of \"" search "\""
              fab {icon: "magnify", small: true, style: M.css {width: 40, position: "absolute", right: 2, bottom: 2}, onPress: RNE.capture_ $ setShowSearch \show -> not show}
           M.view {style: M.css {flex: 1, justifyContent: "flex-end", zIndex: 2}} do
            M.flatList {
              data: images imagesResult,
              renderItem: mkEffectFn1 $ selectableImage selected setSelected,
              keyExtractor: mkEffectFn2 \i n -> pure i.contextLink,
              style: M.css {flex: 2},
              contentContainerStyle: M.css {flex: 2, justifyContent: "flex-end"}, numColumns: 4.0
            }
            button { mode: "contained", onPress: RNE.capture_ $ saveFlashcard mutate mutateWithSentence payload fetch setError (redirectFn selection) params.range selection.book.language setLoading setShouldBlur, disabled: noneSelected selected} $ M.string "Add Images"
      if shouldBlur then unpermittedBlur setModalVisible else mempty

  where redirectFn selection sentenceId =
          runEffectFn2 props.navigation.push "WordSelection" $
            { sentenceId: sentenceId
            , selection: selection
            }
