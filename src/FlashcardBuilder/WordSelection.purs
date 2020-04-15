module FlashcardBuilder.WordSelection where

import Prelude
import Markup as M
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, listIcon)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
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
import Debug.Trace (spy)

type Props = {route :: {params :: {sentenceId :: Int, selection :: Selection}}, navigation :: {navigate :: EffectFn2 String { selection :: Selection, wordTranslation :: String, rangeTranslation :: String, range :: String, rangeOffset :: Int, word :: String } Unit } }

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
            translation
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

headlineItem text translation false = M.getJsx $ paragraph {} $ M.string $ text
headlineItem text translation true = M.getJsx $ paragraph {} $ M.string $ translation

header (showTranslation /\ setShowTranslation) translation text =
  M.view {style: M.css {height: 100, marginLeft: 15, marginRight: 15}} do
    listItem {titleNumberOfLines: 5, onPress: RNE.capture_ $ setShowTranslation \t -> not t, title: headlineItem text translation showTranslation, right: translateIcon, style: M.css {paddingTop: 10, flex: 1}}

buildJsx props = React.do
  let params = props.route.params
  showTranslation <- useState false
  result <- useData (Proxy :: Proxy Query) query { variables: { sentenceId: params.sentenceId }, errorPolicy: "all" }
  case spy "RESULT" result.state of
       Nothing -> pure $ M.getJsx $ M.text {} $ M.string "Loading"
       Just {sentence: { flashcardExistence: {with, without}, text, translation}} -> pure $ M.getJsx do
         M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface { style: M.css { flex: 1 } } do
            M.childElement WordGrid.reactComponent {words: sortWith _.offset without, redirect: redirectFn text translation params.selection, header: M.getJsx $ header showTranslation translation text}
  where redirectFn text translation selection word wordTranslation offset =
          runEffectFn2 props.navigation.navigate "ImageChoice" $
            { wordTranslation: wordTranslation
            , rangeOffset: offset
            , selection: selection
            , range: text
            , rangeTranslation: translation
            , word: word
            }
