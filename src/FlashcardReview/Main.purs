module FlashcardReview.Main where

import Prelude
import React.Basic.Hooks as React
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext, keyed)
import Context (dataStateContext, Context)
import Data.Traversable (traverse_)
import Effect (Effect)
import StackSwiper (cardStack, card)
import Markup as M
import Data.Array (mapWithIndex, (!!), snoc, take, (:), length)
import Effect.Unsafe (unsafePerformEffect)
import FlashcardReview.CardItem as CardItem
import Data.FoldableWithIndex (foldlWithIndexDefault)
import Data.Foldable (foldl)
import Dimensions (window)
import WhiteImageBackground (whiteImageBackground)
import QueryHooks (useData, UseData, stripGraphqlError)
import Effect.Exception (message)
import Type.Proxy (Proxy(..))
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust)
import ApolloHooks (useMutation, gql)
import Debug.Trace (spy)
import Effect.Console (log)
import Effect.Uncurried (mkEffectFn1, mkEffectFn2)
import ComponentTypes (Flashcard, StateChange)
import Ebisu (lowest, randomLow, updateRecall, nRandom)
import Data.Either (Either(..))
import Data.Array.NonEmpty (fromArray, toArray, NonEmptyArray)
import Data.Array as Array
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Data.Set (fromFoldable, delete, member, Set, empty, toUnfoldable, insert, difference, isEmpty, size)
import Navigation (useFocusEffect)
import Data.Map (Map, update, lookup)
import Data.Map as Map
import Segment (track, screen)
import Dimensions (window)
import Icon (materialIcon)
import Animated as Animated
import Animated (AnimationXY, interpolate, value)
import Data.Tuple.Native (T2, t2)
import AsyncStorage (getItem, setItem)
import Data.Interpolate (i)

type Props
  = { navigation :: { navigate :: EffectFn2 String {} Unit }, route :: {params :: {existingIds :: Maybe (Array String), flashcards :: Maybe (NonEmptyArray Flashcard)}} }

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

mutationForIncrement = gql """
  mutation updateDailySessions {
    updateDailySessions {
      result
    }
  }
"""

cardJsx setIsFlipped isFlipped swipeLeft swipeRight i cardData = do
  \active ->
    M.childElement CardItem.reactComponent $
      { setIsFlipped: setIsFlipped
      , isFlipped: isFlipped
      , imageUrl: flashcard.imageUrl
      , word: flashcard.word
      , sentence: flashcard.sentence.text
      , offset: flashcard.startOffset
      , onPressLeft: swipeLeft
      , onPressRight: swipeRight
      , index: i
      , audioUrl: flashcard.sentence.audioUrl
      , sentenceId: flashcard.sentence.id
      , active: active
      , translation: flashcard.sentence.translation
      }
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

setSwipeState result idsNeedingReview setIdsNeedingReview idsInStack setIdsInStack {x: flashcard, y: prediction} = do
  setItem "HasSwiped" "true"
  let reviewWithoutCurrent = if result then delete flashcard.id idsNeedingReview else spy "FIRST" idsNeedingReview
  let stackWithoutCurrent = delete flashcard.id idsInStack
  liftEffect $ setIdsNeedingReview \_ -> reviewWithoutCurrent
  liftEffect $ setIdsInStack \_ -> stackWithoutCurrent
  pure $ {stackWithoutCurrent, reviewWithoutCurrent}

conditionallyUpdateRecall result timesIncorrect setTimesIncorrect {x: flashcard, y: prediction} = do
  liftEffect $ if result then mempty else setTimesIncorrect (\incorrectMap -> incrementIncorrect flashcard incorrectMap)
  let flashcardEbisu = flashcard.a /\ flashcard.b /\ flashcard.t
  let recall = if not result && threeIncorrect flashcard timesIncorrect
    then flashcardEbisu
    else updateRecall flashcardEbisu result flashcard.hoursPassed
  pure $ recall

  where incrementIncorrect flashcard incorrectMap = update (\value -> Just $ value + 1) flashcard.id incorrectMap
        threeIncorrect flashcard timesIncorrect = fromMaybe false do
            incorrectForFlashcard <- lookup flashcard.id timesIncorrect
            pure $ incorrectForFlashcard >= 3

saveRecall mutate (a /\ b /\ t) stackWithoutCurrent reviewWithoutCurrent {x: flashcard, y: prediction} = do
  try $ mutate
    {
      variables:
        {
         input:
          {
            flashcardId: flashcard.id
          , a: a
          , b: b
          , t: t
          , inStack: toUnfoldable stackWithoutCurrent :: Array String
          , returnedFlashcardIds: toUnfoldable $ difference reviewWithoutCurrent stackWithoutCurrent :: Array String
          }
        }
    }

handleResponse incrementSessionsMutation responseOrError redirect setError stackWithoutCurrent idsInStack setIdsInStack setCardList = do
  case responseOrError of
      Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
      Right resp -> addCard (fromArray (spy "RESP" resp).updateFlashcard.flashcards) stackWithoutCurrent
  where
        addCard (Just cards) stack = do
          newCard <- liftEffect $ randomLow cards
          traverse_ (\n -> liftEffect $ setIdsInStack \s -> insert n.x.id s) newCard
          traverse_ (\n -> liftEffect $ setCardList \cards -> snoc cards n) newCard
        addCard Nothing stack
          | isEmpty stack = do
              _ <- track "Completed Session" {}
              _ <- incrementSessionsMutation {}
              liftEffect $ runEffectFn2 redirect "ReviewComplete" {}
          | otherwise = mempty

leftCircleStyle drag =
  { width: 60
  , height: 60
  , borderRadius: 60/2
  , borderWidth: 2
  , borderColor: "red"
  , position: "absolute"
  , left: 0
  , backgroundColor: "white"
  , top: window.height / 3.0
  , zIndex: 2
  , justifyContent: "center"
  , opacity: interpolate drag
      { inputRange: [-500.0, -90.0, 0.0]
      , outputRange: [0.0, 0.6, 0.0]
      , extrapolate: "clamp"
      }
  , alignItems: "center"
  , transform: t2
       { translateX: interpolate (spy "DRAG" drag)
          { inputRange: [-500.0, -90.0, 0.0]
          , outputRange: [0.0, 80.0, 0.0]
          , extrapolate: "clamp"
          }
       }
       { scale: interpolate drag
            { inputRange: [-500.0, 0.0]
            , outputRange: [1.2, 1.0]
            , extrapolate: "clamp"
            }
        }

  }

rightCircleStyle drag =
  { width: 60
  , height: 60
  , borderRadius: 60/2
  , borderColor: "green"
  , borderWidth: 2
  , position: "absolute"
  , right: 0
  , backgroundColor: "white"
  , top: window.height / 3.0
  , opacity: interpolate drag
      { inputRange: [0.0, 90.0, 500.0]
      , outputRange: [0.0, 0.6, 0.0]
      , extrapolate: "clamp"
      }

  , zIndex: 2
  , justifyContent: "center"
  , alignItems: "center"
  , transform: t2
      {
        translateX: interpolate (spy "DRAG" drag)
          { inputRange: [0.0, 90.0, 500.0]
          , outputRange: [0.0, -80.0, 0.0]
          , extrapolate: "clamp"
          }
      }
      { scale: interpolate drag
            { inputRange: [0.0, 500.0]
            , outputRange: [1.0, 1.2]
            , extrapolate: "clamp"
            }
        }
  }

dragFn :: StateChange (Maybe AnimationXY) -> AnimationXY -> Effect Unit
dragFn setDrag drag = setDrag \_ -> Just $ spy "DRAG" drag


buildJsx props = React.do
  swipeRef <- useRef null
  { setLoading, setError } <- useContext dataStateContext

  mutate /\ d1 <- useMutation mutation { errorPolicy: "all" }
  incrementMutation /\ d2 <- useMutation mutationForIncrement { errorPolicy: "all" }
  cardList /\ setCardList <- useState ([] :: Array {x :: Flashcard, y :: Number})
  idsNeedingReview /\ setIdsNeedingReview <- useState (empty :: Set String)
  isFlipped /\ setIsFlipped <- useState false
  timesIncorrect /\ setTimesIncorrect <- useState (Map.empty :: Map String Int)
  idsInStack /\ setIdsInStack <- useState (empty :: Set String)
  drag /\ setDrag <- useState (Nothing :: Maybe AnimationXY)

  let swipeLeft = do
        result <- readRefMaybe swipeRef
        traverse_ (\s -> s.swipeLeft) result

  let swipeRight = do
        result <- readRefMaybe swipeRef
        traverse_ (\s -> s.swipeRight) result

  useEffect unit do
     launchAff_ $ do
        screen "Flashcard Review" {}
     pure mempty
  useEffect (isJust props.route.params.flashcards) $ do
    case props.route.params.flashcards of
         Nothing -> mempty
         Just flashcards -> do
          flashcardArray <-
            case props.route.params.existingIds of
                 Nothing -> do
                    flashcardsToReview <- nRandom 30 flashcards
                    pure $ Array.fromFoldable flashcardsToReview
                 Just ids -> do
                    let set = spy "SET" $ fromFoldable ids
                    let newAccum accum flashcard = if member flashcard.id set
                            then {x: flashcard, y: 0.0} : accum
                            else accum
                    pure $ foldl newAccum [] flashcards
          let firstThree = spy "FIRST THREE" $ take 3 $ spy "ARRAY" flashcardArray
          setIdsNeedingReview \_ -> fromFoldable $ map (\e -> e.x.id) flashcardArray
          setTimesIncorrect \_ -> foldl (\accum e -> Map.insert e.x.id 0 accum) Map.empty flashcardArray
          setIdsInStack \_ -> fromFoldable $ map (\e -> e.x.id) firstThree
          setCardList \_ -> firstThree
    pure mempty

  let afterSwipe Nothing result index = mempty
      afterSwipe (Just card) result index = launchAff_ do
        {stackWithoutCurrent, reviewWithoutCurrent} <- setSwipeState result idsNeedingReview setIdsNeedingReview idsInStack setIdsInStack card
        recall <- conditionallyUpdateRecall result timesIncorrect setTimesIncorrect card
        response <- saveRecall mutate recall stackWithoutCurrent reviewWithoutCurrent card
        handleResponse incrementMutation response props.navigation.navigate setError stackWithoutCurrent idsInStack setIdsInStack setCardList
  let cardsMarkup cards = whiteImageBackground {style: M.css imageBackgroundStyles} do
          Animated.view {style: leftCircleStyle $ fromMaybe (value 0.0) $ _.x <$> drag} do
             M.text {style: M.css {color: "red", fontSize: 26}} $ M.string "x"
          Animated.view {style: M.css $ rightCircleStyle $ fromMaybe (value 0.0) $ _.x <$> drag} do
             materialIcon {name: "check", color: "green", size: 26}
          M.view {style: M.css { marginHorizontal: 10, height: window.height }} do
            cardStack
              { onSwipedLeft: mkEffectFn1 \i -> afterSwipe (cards !! (spy "i LEFT" i)) false i
              , onSwipedRight: mkEffectFn1 \i -> afterSwipe (cards !! (spy "i RIGHT" i)) true i
              , setDrag: mkEffectFn1 $ dragFn setDrag
              , verticalSwipe: false
              , horizontalSwipe: isFlipped
              , ref: swipeRef
              , renderNoMoreCards: (\_ -> false)
              } $ mapWithIndex (cardJsx setIsFlipped isFlipped swipeLeft swipeRight) $ spy "FLASHCARDS" cards
  pure $ M.getJsx do
     M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
        cardsMarkup (spy "LIST" cardList)
        if isEmpty idsNeedingReview then mempty else
          M.view { style: M.css {width: "100%", bottom: 0, position: "absolute", alignItems: "center", justifyContent: "center", backgroundColor: "white", height: 45}} do
            M.text {style: M.css {marginBottom: 5, marginTop: 5}} $ M.string $ i (30 - (size idsNeedingReview)) " / 30 cards completed"
