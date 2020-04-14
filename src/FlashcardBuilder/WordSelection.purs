module FlashcardBuilder.WordSelection where

import Prelude
import Markup as M
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, listIcon)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Effect.Unsafe (unsafePerformEffect)
import Record.Unsafe.Union (unsafeUnion)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, mkEffectFn2, runEffectFn2, EffectFn2)
import ComponentTypes (Selection)
import Data.String (split, Pattern(..), trim)
import Dimensions (window)
import ApolloHooks (useMutation, gql)
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import Data.Array (sortWith)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import Debug.Trace (spy)

type Props = {route :: {params :: {sentenceId :: Int}}, navigation :: {navigate :: EffectFn2 String { selection :: Selection, wordTranslation :: String, rangeTranslation :: String, range :: String, rangeOffset :: Int } Unit } }

type FlashcardOffset = {word :: String, offset :: Int}
type FlashcardExistence = {with :: Array FlashcardOffset, without :: Array FlashcardOffset}
type Sentence
  = {text :: String, translation :: String, flashcardExistence :: FlashcardExistence}

type Query = {sentence :: Sentence}

query =
  gql
    """
    query getFlashcards($sentenceId: ID) {
      sentence(id: $sentenceId) {
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
          }
        }
      }
    }
"""

translateIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "google-translate" }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "WordSelection" $ buildJsx

wordElement w = pure $ M.getJsx do
  M.view {style: M.css {padding: 20, borderWidth: 1, flex: 1}} do
    M.text {} $ M.string w.item.word

headlineItem text translation false = M.getJsx $ paragraph {} $ M.string $ text
headlineItem text translation true = M.getJsx $ paragraph {} $ M.string $ translation

buildJsx props = React.do
  let params = props.route.params
  showTranslation /\ setShowTranslation <- useState false
  result <- useData (Proxy :: Proxy Query) query { variables: { sentenceId: params.sentenceId }, errorPolicy: "all" }
  case spy "RESULT" result.state of
       Nothing -> pure $ M.getJsx $ M.text {} $ M.string "Loading"
       Just {sentence: { flashcardExistence: {with, without}, text, translation}} -> pure $ M.getJsx do
         M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface { style: M.css { flex: 1 } } do
              M.view {style: M.css {flex: 1, marginLeft: 15, marginRight: 15}} do

                listItem {titleNumberOfLines: 5, onPress: RNE.capture_ $ setShowTranslation \t -> not t, title: headlineItem text translation showTranslation, right: translateIcon, style: M.css {paddingTop: 10}}
                M.view {style: M.css {flex: 1} } do
                  M.flatList {
                    data: sortWith _.offset without,
                    renderItem: mkEffectFn1 $ wordElement,
                    style: M.css {flex: 1},
                    contentContainerStyle: M.css {flex: 1, padding: 10, justifyContent: "space-between", flexDirection: "row"}, numColumns: 2.0
                  }
