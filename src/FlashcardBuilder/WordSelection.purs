module FlashcardBuilder.WordSelection where

import Prelude
import Markup as M
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, listIcon, portal, dialogTitle, dialogActions, dialogContent, dialog, caption)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element, useLayoutEffect, Render, UseState)
import Effect.Unsafe (unsafePerformEffect)
import Record.Unsafe.Union (unsafeUnion)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, mkEffectFn2, runEffectFn2, EffectFn2)
import ComponentTypes (Selection, Sentence)
import Data.String (split, Pattern(..), trim)
import Dimensions (window)
import ApolloHooks (useMutation, gql)
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Array (sortWith)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import FlashcardBuilder.WordGrid as WordGrid
import Effect (Effect)
import Debug.Trace (spy)

type NavigateFn = String -> {} -> Effect Unit
type ImageNavigation =
  { audio :: Maybe String
  , existingSentence :: Boolean
  , range :: String
  , rangeOffset :: Int
  , rangeTranslation :: String
  , selection :: Selection
  , word :: String
  , wordTranslation :: String
  }
type Navigation = {push :: EffectFn2 String ImageNavigation Unit, setOptions :: EffectFn1 {headerLeft :: ReactComponent {}} Unit, navigate :: EffectFn2 String {} Unit}
type Props  = {route :: {params :: {sentenceId :: Int, selection :: Selection}}, navigation :: Navigation}

type Query = {sentence :: Sentence}

query =
  gql
    """
    query getSentences($sentenceId: ID) {
      sentence(id: $sentenceId) {
        audioUrl
        text
        translation
        flashcardExistence {
          with {
            word
            offset
          }
          without {
            word
            offset
            translation
          }
        }
      }
    }
"""

buttonComponent :: (String -> {} -> Effect Unit) -> ReactComponent {}
buttonComponent navigate = unsafePerformEffect
    $ do
        (component "CloseButton") $ buttonJsx navigate

buttonJsx navigate props = React.do
  pure $ M.getJsx do
    button {onPress: RNE.capture_ $ navigate "WordList" {}, style: M.css {position: "absolute", left: 1, marginTop: 21}} $ M.string "Close"

translateIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "google-translate" }

reactComponent :: ReactComponent (Props)
reactComponent =
  unsafePerformEffect
    $ do
        component "WordSelection" $ buildJsx

headlineItem text translation false = M.getJsx $ paragraph {} $ M.string $ text
headlineItem text translation true = M.getJsx $ paragraph {} $ M.string $ translation

header (showTranslation /\ setShowTranslation) translation text =
  M.view {style: M.css {height: 100, marginLeft: 15, marginRight: 15}} do
    listItem {titleNumberOfLines: 7, onPress: RNE.capture_ $ setShowTranslation \t -> not t, title: headlineItem text translation showTranslation, right: translateIcon, style: M.css {paddingTop: 10, flex: 1}}

buildJsx props = React.do
  let params = props.route.params
  let navigate = runEffectFn2 props.navigation.navigate
  showTranslation <- useState false

  dialogVisible /\ setDialogVisible <- useState false
  useLayoutEffect unit do
     runEffectFn1 props.navigation.setOptions {headerLeft: buttonComponent navigate }
     pure mempty
  result <- useData (Proxy :: Proxy Query) query { variables: { sentenceId: params.sentenceId }, errorPolicy: "all", fetchPolicy: "cache-and-network" }
  pure $ M.getJsx do
    case spy "RESULT" result.state of
        Nothing -> mempty
        Just {sentence: { flashcardExistence: {with, without}, text, translation, audioUrl}} ->
          M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
            surface { style: M.css { flex: 1 } } do
              M.view {style: M.css {alignItems: "center"}} do
                caption {style: M.css {}} $ M.string "Optionally create more flashcards from words in the sentence"
              M.childElement WordGrid.reactComponent {words: sortWith _.offset without, redirect: redirectFn text translation params.selection audioUrl, header: M.getJsx $ header showTranslation translation text}
    where redirectFn text translation selection audio word wordTranslation offset =
            runEffectFn2 props.navigation.push "ImageChoice" $
              { wordTranslation: wordTranslation
              , rangeOffset: offset
              , selection: selection
              , range: text
              , rangeTranslation: translation
              , word: word
              , existingSentence: true
              , audio: Just audio
              }
