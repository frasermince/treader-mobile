module FlashcardBuilder.ImageChoice where

import Prelude
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon)
import Record.Unsafe.Union (unsafeUnion)
import ImageSearch (imageSearch, Image)
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
import Data.Maybe (fromMaybe)
import MaterialIcon (icon)
import TextToSpeech (speak, setDefaultLanguage)
import Data.Map (fromFoldable, lookup)
import Data.Traversable (traverse_)
import Data.FoldableWithIndex (foldlWithIndexDefault)
import Data.Foldable(foldl)
import ComponentTypes (Selection)
import Data.String (stripPrefix, Pattern(..))
import Data.Array ((:))
import Ebisu as Ebisu
import KeyboardAwareDialog (keyboardAwareDialog)

type Props = {route :: {params :: {selection :: Selection, range :: String, wordTranslation :: String, rangeTranslation :: String, rangeOffset :: Int, word :: String}}, navigation :: {navigate :: EffectFn2 String { sentenceId :: Int, selection :: Selection } Unit } }

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

getImages setSelected keyword setImages setError = launchAff_ do
  liftEffect $ setSelected \_ -> replicate 8 false
  liftEffect $ setImages \_ -> []
  result <- try $ imageSearch keyword 0 8
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ message error
    Right images -> liftEffect $ setImages \_ -> images

selectedImages selectedIndices images =
  foldlWithIndexDefault foldFn [] images
  where foldFn index accum image = if isSelected selectedIndices index then image.image.thumbnailLink : accum else accum

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
    image {style: M.css $ imageStyle (isSelected selected index), source: {uri: i.item.image.thumbnailLink}}

stripGraphqlError message = fromMaybe message $ stripPrefix (Pattern "GraphQL error: ") message

makePayload selection range rangeTranslation rangeOffset imageUrl word =
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
        sentenceTranslation: rangeTranslation
      }
    }
  }
  where a /\ b /\ t = Ebisu.defaultModel 24.0

saveFlashcard mutate payload setError redirect = launchAff_ do
  result <- try $ mutate $ payload
  case result of
    Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
    Right resp -> liftEffect $ redirect resp.createFlashcard.flashcard.sentenceId

searchFromDialog setShowSearch setSelected search setImages setError = do
  setShowSearch \_ -> false
  getImages setSelected search setImages setError

mutation =
  gql
    """
mutation flashcardMutation($input: LoginInput!) {
  createFlashcard(input: $input) {
    flashcard {sentenceId}
  }
}
  """

noneSelected selected = foldl (\accum s -> accum && not s) true selected

paragraphItem params false = M.getJsx $ paragraph {} $ M.jsx $ [ underlineWord params.range params.rangeOffset]
paragraphItem params true = M.getJsx $ paragraph {} $ M.string $ params.rangeTranslation

buildJsx props = React.do
  { setLoading, setError } <- useContext dataStateContext
  mutate /\ d <- useMutation mutation { errorPolicy: "all" }
  let params = props.route.params
  let selection = params.selection
  selected /\ setSelected <- useState $ replicate 8 false
  search /\ setSearch <- useState params.word
  images /\ setImages <- useState ([] :: Array Image)
  showSearch /\ setShowSearch <- useState false
  showTranslation /\ setShowTranslation <- useState false
  useEffect selection.book.language do
    traverse_ setDefaultLanguage $ lookup selection.book.language languageList
    pure mempty
  useEffect params.word do
    getImages setSelected params.word setImages setError
    pure mempty

  let payload = makePayload selection params.range params.rangeTranslation params.rangeOffset (selectedImages selected images) params.word
  pure $ M.getJsx do
    portal {} do
      keyboardAwareDialog {visible: showSearch, onDismiss: setShowSearch \_ -> false} do
        dialogTitle {} $ M.string "Search Google For Images"
        dialogContent {} do
            textInput { placeholder: "Search", onChangeText: changeField setSearch, value: search }
        dialogActions {} do
            button {onPress: RNE.capture_ $ setShowSearch \_ -> false} $ M.string "Cancel"
            button {onPress: RNE.capture_ $ searchFromDialog setShowSearch setSelected search setImages setError} $ M.string "Search"

    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      surface { style: M.css { flex: 1 } } do
         M.view {style: M.css {flex: 2, justifyContent: "flex-end", marginLeft: 15}} do
           headline {} $ M.string params.word
           M.text {style: M.css {marginBottom: 30}} $ M.string params.wordTranslation
         M.view {style: M.css {flex: 6}} do
           M.view {style: M.css {flex: 4}} do
            divider {style: M.css {height: 1, width: "100%"}}
            M.view {style: M.css {paddingTop: "5%", paddingLeft: 5, paddingRight: 5}} do
              fab {icon: "volume-medium", small: true, style: M.css {width: 40}, onPress: RNE.capture_ $ speak params.range {}}
              listItem {titleNumberOfLines: 5, onPress: RNE.capture_ $ setShowTranslation \t -> not t, title: paragraphItem params showTranslation, right: translateIcon, style: M.css {paddingTop: 10}}
            --paragraph {style: M.css {paddingTop: 20, paddingLeft: 5, paddingRight: 5}} $ M.string $ params.rangeTranslation
            fab {icon: "magnify", small: true, style: M.css {width: 40, position: "absolute", right: 2, bottom: 2}, onPress: RNE.capture_ $ setShowSearch \show -> not show}
           M.view {style: M.css {flex: 3, justifyContent: "flex-end", zIndex: 2}} do
            M.flatList {
              data: images,
              renderItem: mkEffectFn1 $ selectableImage selected setSelected,
              keyExtractor: mkEffectFn2 \i n -> pure i.link,
              style: M.css {flex: 2},
              contentContainerStyle: M.css {flex: 2, justifyContent: "flex-end"}, numColumns: 4.0
            }
            button { mode: "contained", onPress: RNE.capture_ $ saveFlashcard mutate payload setError $ redirectFn selection, disabled: noneSelected selected} $ M.string "Add Images"
  where redirectFn selection sentenceId =
          runEffectFn2 props.navigation.navigate "WordSelection" $
            { sentenceId: sentenceId
            , selection: selection
            }
