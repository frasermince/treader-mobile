module FlashcardReview.Main where

import Prelude
import React.Basic.Hooks as React
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import Context (dataStateContext, Context)
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
import QueryHooks (useData, UseData, stripGraphqlError)
import Effect.Exception (message)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import ApolloHooks (useMutation, gql)
import Debug.Trace (spy)
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import ComponentTypes (Flashcard)
import Ebisu (lowestThree, randomLow, updateRecall)
import Data.Either (Either(..))
import Data.Array.NonEmpty (fromArray, toArray)
import Data.Array (snoc)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, EffectFn1)

type Props = {}

type Query = {flashcards :: Array Flashcard}

mutation = gql """
  mutation flashcardMutation($input: UpdateFlashcardInput!) {
    updateFlashcard(input: $input) {
      flashcard {
        id
        a
        b
        t
        sentence {
          id
          lastReviewed
        }
      }
    }
  }
"""

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
          hoursPassed
          translation
        }
      }
    }
"""

cardJsx setIsFlipped isFlipped swipeLeft swipeRight i cardData = do
  \active -> M.childElement CardItem.reactComponent {setIsFlipped: setIsFlipped, isFlipped: isFlipped, imageUrl: flashcard.imageUrl, word: flashcard.word, sentence: flashcard.sentence.text, offset: flashcard.startOffset, onPressLeft: swipeLeft, onPressRight: swipeRight, index: i, audioUrl: flashcard.sentence.audioUrl, sentenceId: flashcard.sentence.id, active: active}
  where flashcard = cardData.x

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

handleSwipe result mutate flashcard setError setCards = launchAff_ do
  let flashcardEbisu = flashcard.a /\ flashcard.b /\ flashcard.t
  let (a /\ b /\ t) = updateRecall flashcardEbisu result flashcard.hoursPassed
  result <- try $ mutate {variables: {input: {flashcardId: flashcard.id, a: a, b: b, t: t}}}
  case result of
       Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
       Right resp -> do
          addCard $ fromArray resp.updateFlashcard.cards

  where addCard (Just cards) = do
          newCard <- liftEffect $ randomLow cards 
          setCards \cards -> snoc cards newCard
        addCard Nothing = mempty


buildJsx props = React.do
  swipeRef <- useRef null
  { setLoading, setError } <- useContext dataStateContext
  flashcardsResult <- useData (Proxy :: Proxy Query) query { errorPolicy: "all", fetchPolicy: "cache-and-network" }

  mutate /\ d <- useMutation mutation { errorPolicy: "all" }
  cards /\ setCards <- useState ([] :: Array {x :: Flashcard, y :: Number})
  isFlipped /\ setIsFlipped <- useState false

  let swipeLeft = do
        result <- readRefMaybe swipeRef
        traverse_ (\s -> s.swipeLeft) result

  let swipeRight = do
        result <- readRefMaybe swipeRef
        traverse_ (\s -> s.swipeRight) result

  useEffect flashcardsResult.state $ do
    case (_.flashcards <$> flashcardsResult.state) >>= fromArray of
         Nothing -> mempty
         Just flashcards -> setCards \_ -> lowestThree flashcards
    pure mempty

  pure $ M.getJsx do
    M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      whiteImageBackground {style: M.css imageBackgroundStyles} do
        M.view {style: M.css { marginHorizontal: 10, height: window.height }} do
          spy "STACK" cardStack {onSwiped: mkEffectFn1 $ swiped setIsFlipped, verticalSwipe: false, horizontalSwipe: isFlipped, ref: swipeRef, renderNoMoreCards: (\_ -> false)} $ mapWithIndex (cardJsx setIsFlipped isFlipped swipeLeft swipeRight) $ spy "FLASHCARDS" cards
