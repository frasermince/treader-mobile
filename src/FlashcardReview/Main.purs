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
import Data.Array (mapWithIndex, (!!), snoc, take)
import Effect.Unsafe (unsafePerformEffect)
import FlashcardReview.CardItem as CardItem
import Data.FoldableWithIndex (foldlWithIndexDefault)
import Dimensions (window)
import WhiteImageBackground (whiteImageBackground)
import QueryHooks (useData, UseData, stripGraphqlError)
import Effect.Exception (message)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust)
import ApolloHooks (useMutation, gql)
import Debug.Trace (spy)
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1)
import ComponentTypes (Flashcard)
import Ebisu (lowest, randomLow, updateRecall)
import Data.Either (Either(..))
import Data.Array.NonEmpty (fromArray, toArray)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, EffectFn1)
import Data.Set (fromFoldable, delete, member, Set, empty, toUnfoldable, insert, difference)
import Navigation (useFocusEffect)

type Props = {}

type Query = {flashcards :: Array Flashcard}

mutation = gql """
  mutation flashcardMutation($input: UpdateFlashcardInput!) {
    updateFlashcard(input: $input) {
      flashcards {
        id
        imageUrl
        a
        b
        t
        startOffset
        word
        hoursPassed
        sentence {
          id
          audioUrl
          text
          translation
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
        id
        imageUrl
        a
        b
        t
        startOffset
        word
        hoursPassed
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

handleSwipe mutate setError setCardList idsNeedingReview setIdsNeedingReview idsInStack setIdsInStack Nothing result index = mempty
handleSwipe mutate setError setCardList idsNeedingReview setIdsNeedingReview idsInStack setIdsInStack (Just {x: flashcard, y: prediction}) result index = launchAff_ do
  let reviewWithoutCurrent = if result then delete flashcard.id idsNeedingReview else idsNeedingReview
  let stackWithoutCurrent = delete flashcard.id idsInStack
  liftEffect $ setIdsNeedingReview \_ -> reviewWithoutCurrent
  liftEffect $ setIdsInStack \_ -> stackWithoutCurrent
  let flashcardEbisu = flashcard.a /\ flashcard.b /\ flashcard.t
  let (a /\ b /\ t) = updateRecall flashcardEbisu result flashcard.hoursPassed
  result <- try $ mutate {variables: {input: {flashcardId: flashcard.id, a: a, b: b, t: t, returnedFlashcardIds: toUnfoldable $ difference reviewWithoutCurrent stackWithoutCurrent :: Array String}}}
  case result of
       Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
       Right resp -> addCard $ fromArray resp.updateFlashcard.flashcards
  where addCard (Just cards) = do
          newCard <- liftEffect $ randomLow cards
          liftEffect $ setIdsInStack \s -> insert newCard.x.id s
          liftEffect $ setCardList \cards -> snoc cards newCard
        addCard Nothing = mempty

buildJsx props = React.do
  swipeRef <- useRef null
  { setLoading, setError } <- useContext dataStateContext
  flashcardsResult <- useData (Proxy :: Proxy Query) query { errorPolicy: "all", fetchPolicy: "cache-and-network" }

  useFocusEffect unit do
     flashcardsResult.refetch {}
     pure mempty

  mutate /\ d <- useMutation mutation { errorPolicy: "all" }
  cardList /\ setCardList <- useState ([] :: Array {x :: Flashcard, y :: Number})
  idsNeedingReview /\ setIdsNeedingReview <- useState (empty :: Set String)
  isFlipped /\ setIsFlipped <- useState false
  idsInStack /\ setIdsInStack <- useState (empty :: Set String)

  let swipeLeft = do
        result <- readRefMaybe swipeRef
        traverse_ (\s -> s.swipeLeft) result

  let swipeRight = do
        result <- readRefMaybe swipeRef
        traverse_ (\s -> s.swipeRight) result

  useEffect (isJust flashcardsResult.state) $ do
    case (_.flashcards <$> flashcardsResult.state) >>= fromArray of
         Nothing -> mempty
         Just flashcards -> do
          let l = lowest flashcards
          let firstThree = take 3 $ l
          setIdsNeedingReview \_ -> fromFoldable $ map (\e -> e.x.id) l
          setIdsInStack \_ -> fromFoldable $ map (\e -> e.x.id) firstThree
          setCardList \_ -> firstThree
    pure mempty

  let afterSwipe = handleSwipe mutate setError setCardList idsNeedingReview setIdsNeedingReview idsInStack setIdsInStack
  pure $ M.getJsx do
     M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
      case cardList of
            [] -> M.view {style: M.css {flex: 1, justifyContent: "center", alignItems: "center"}} do
              M.text {style: M.css {}} $ M.string "Create cards to review them here"
            cards -> whiteImageBackground {style: M.css imageBackgroundStyles} do
                      M.view {style: M.css { marginHorizontal: 10, height: window.height }} do
                        cardStack {onSwipedLeft: mkEffectFn1 \i -> afterSwipe (cards !! i) false i, onSwipedRight: mkEffectFn1 \i -> afterSwipe (cards !! i) true i, verticalSwipe: false, horizontalSwipe: isFlipped, ref: swipeRef, renderNoMoreCards: (\_ -> false)} $ mapWithIndex (cardJsx setIsFlipped isFlipped swipeLeft swipeRight) $ spy "FLASHCARDS" cards
