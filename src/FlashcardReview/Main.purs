module FlashcardReview.Main where

import Prelude
import React.Basic.Hooks as React
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import Data.Traversable (traverse_)
import Effect (Effect)
import StackSwiper (cardStack, card)
import Markup as M
import Data.Array (mapWithIndex)
import Effect.Unsafe (unsafePerformEffect)
import FlashcardReview.CardItem as CardItem
import Data.FoldableWithIndex (foldlWithIndexDefault)
import Dimensions (window)
import WhiteImageBackground (whiteImageBackground)
import QueryHooks (useData, UseData)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import ApolloHooks (useMutation, gql)
import Debug.Trace (spy)
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)

type Props = {}

type Flashcard = {word :: String, sentence :: {text :: String, translation :: String, audioUrl :: String, id :: String}, imageUrl :: Array String, a :: Number, b :: Number, t :: Number, startOffset :: Int}
type Query = {flashcards :: Array Flashcard}

query =
  gql
    """
    query getFlashcards {
      flashcards {
        imageUrl
        a
        b
        t
        startOffset
        word
        sentence {
          id
          audioUrl
          text
          translation
        }
      }
    }
"""

cardJsx setIsFlipped isFlipped swipeLeft swipeRight i cardData = do
  \active -> M.childElement CardItem.reactComponent {setIsFlipped: setIsFlipped, isFlipped: isFlipped, imageUrl: cardData.imageUrl, word: cardData.word, sentence: cardData.sentence.text, offset: cardData.startOffset, onPressLeft: swipeLeft, onPressRight: swipeRight, index: i, audioUrl: cardData.sentence.audioUrl, sentenceId: cardData.sentence.id, active: active}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Review" $ buildJsx

imageBackgroundStyles = {
  flex: 1,
  resizeMode: "cover",
  width: window.width,
  height: window.height
}

swiped setIsFlipped index = do
  log "TEST"

buildJsx props = React.do
  let cards = [
      {word: "etre", sentenceText: "Etre ou ne etre pas", imageUrl: ["https://google.com"]},
      {word: "etre", sentenceText: "Etre ou ne etre pas", imageUrl: ["https://google.com"]}
  ]
  swipeRef <- useRef null
  result <- useData (Proxy :: Proxy Query) query { errorPolicy: "all", fetchPolicy: "cache-and-network" }
  isFlipped /\ setIsFlipped <- useState false

  let swipeLeft = do
        result <- readRefMaybe swipeRef
        traverse_ (\s -> s.swipeLeft) result

  let swipeRight = do
        result <- readRefMaybe swipeRef
        traverse_ (\s -> s.swipeRight) result

  case result.state of
       Nothing -> mempty
       Just {flashcards: flashcards} -> pure $ M.getJsx do
        M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          whiteImageBackground {style: M.css imageBackgroundStyles} do
            M.view {style: M.css { marginHorizontal: 10, height: window.height }} do
              spy "STACK" cardStack {onSwiped: mkEffectFn1 $ swiped setIsFlipped, verticalSwipe: false, horizontalSwipe: isFlipped, ref: swipeRef, renderNoMoreCards: (\_ -> false)} $ mapWithIndex (cardJsx setIsFlipped isFlipped swipeLeft swipeRight) $ spy "FLASHCARDS" flashcards
