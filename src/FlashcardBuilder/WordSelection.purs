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
import Data.Array (head, notElem, filter)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE

type Props = {route :: {params :: {sentenceId :: Int}}, navigation :: {navigate :: EffectFn2 String { selection :: Selection, wordTranslation :: String, rangeTranslation :: String, range :: String, rangeOffset :: Int } Unit } }

type Query
  = {flashcards :: Array {sentence :: {text :: String, translation :: String}, word :: String}}

query =
  gql
    """
    query getFlashcards($sentenceId: ID) {
      flashcards(sentenceId: $sentenceId) {
        word
        sentence {
          text
          translation
        }
      }
    }
"""

translateIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "google-translate" }

getResults queryResult = do
  r <- queryResult
  firstFlashcard <- head r.flashcards
  pure $ {sentence: firstFlashcard.sentence, flashcards: r.flashcards}
reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "WordSelection" $ buildJsx

wordElement word = pure $ M.getJsx do
  M.view {style: M.css {padding: 20, borderWidth: 1, flex: 1}} do
    M.text {} $ M.string word.item

headlineItem sentence false = M.getJsx $ paragraph {} $ M.string $ sentence.text
headlineItem sentence true = M.getJsx $ paragraph {} $ M.string $ sentence.translation

wordsWithoutFlashcards flashcards word = notElem word flashcards

buildJsx props = React.do
  let params = props.route.params
  showTranslation /\ setShowTranslation <- useState false
  result <- useData (Proxy :: Proxy Query) query { variables: { sentenceId: params.sentenceId }, errorPolicy: "all" }
  case getResults $ result.state of
       Nothing -> pure $ M.getJsx $ M.text {} $ M.string "Loading"
       Just {flashcards: flashcards, sentence: sentence} -> pure $ M.getJsx do
         let flashcardWords = map _.word flashcards
         let words = filter (wordsWithoutFlashcards flashcardWords) $ split (Pattern " ") sentence.text
         M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface { style: M.css { flex: 1 } } do
              M.view {style: M.css {flex: 1, marginLeft: 15, marginRight: 15}} do

                listItem {titleNumberOfLines: 5, onPress: RNE.capture_ $ setShowTranslation \t -> not t, title: headlineItem sentence showTranslation, right: translateIcon, style: M.css {paddingTop: 10}}
                M.view {style: M.css {flex: 1} } do
                  M.flatList {
                    data: words,
                    renderItem: mkEffectFn1 $ wordElement,
                    style: M.css {flex: 2},
                    contentContainerStyle: M.css {flex: 2, margin: 10}, numColumns: 2.0
                  }
