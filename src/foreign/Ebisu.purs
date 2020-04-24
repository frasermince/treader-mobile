module Ebisu where
import Prelude
import Data.Tuple.Native (T3, t3, prj)
import React.Basic.Hooks ((/\), type(/\))
import Data.Typelevel.Num.Reps (d0, d1, d2)
import ComponentTypes (Flashcard)
import Data.Array.NonEmpty (length, take, (!!), head, NonEmptyArray)
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Random (randomInt)
import Debug.Trace (spy)

type EbisuModelJs = T3 Number Number Number
type EbisuModel = Number /\ Number /\ Number
foreign import _defaultModel :: Number -> EbisuModelJs
foreign import _predictRecall :: EbisuModelJs -> Number -> Number
foreign import _updateRecall :: EbisuModelJs -> Boolean -> Number -> EbisuModelJs

toTuple :: EbisuModelJs -> EbisuModel
toTuple tuple = (prj d0 tuple) /\ (prj d1 tuple) /\ (prj d2 tuple)

defaultModel :: Number -> EbisuModel
defaultModel n = toTuple $ _defaultModel n

predictRecall :: EbisuModel -> Number -> Number
predictRecall (a /\ b /\ t) elapsed = _predictRecall (t3 a b t) elapsed

updateRecall :: EbisuModel -> Boolean -> Number -> EbisuModel
updateRecall (a /\ b /\ t) result elapsed = toTuple $ _updateRecall (t3 a b t) result elapsed

foreign import binLowest :: forall a b . NonEmptyArray a -> Array Number -> (a -> b) -> NonEmptyArray a

foreign import partialSort :: forall a b . NonEmptyArray a -> Int -> (a -> b) -> NonEmptyArray {x :: a, y :: b}

randomLow :: NonEmptyArray Flashcard -> Effect Flashcard
randomLow flashcards = randomLowest binnedLowest
  where binnedLowest :: NonEmptyArray {x :: Flashcard, y :: Number}
        binnedLowest = binLowest (sortedItems flashcards) [1e-3, 1e-2, 1e-1, 5e-1] (\e -> e.y)
        randomLowest l = do
          index <- randomInt 0 (length l - 1)
          case l !! index of
               Just value -> pure value.x
               Nothing -> pure $ (head l).x

lowestThree :: NonEmptyArray Flashcard -> Array {x :: Flashcard, y :: Number}
lowestThree flashcards = take 3 $ spy "FLASHCARDS" $ sortedItems flashcards

sortedItems :: NonEmptyArray Flashcard -> NonEmptyArray {x :: Flashcard, y :: Number}
sortedItems flashcards = partialSort (spy "SORTED" flashcards) numberOfItems sortFn
  where sortFn flashcard = predictRecall (flashcard.a /\ flashcard.b /\ flashcard.t) $ spy "LAST REVIEWED" flashcard.sentence.hoursPassed
        numberOfItems = min 30 (length flashcards)
