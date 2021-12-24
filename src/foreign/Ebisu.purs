module Ebisu where
import Prelude
import Data.Tuple.Native (T3, t3, prj)
import React.Basic.Hooks ((/\), type(/\))
import Data.Typelevel.Num.Reps (d0, d1, d2)
import ComponentTypes (Flashcard)
import Data.Array.NonEmpty (take, (!!), head, NonEmptyArray, toArray, updateAt, fromArray)
import Data.Array.NonEmpty as NonEmpty
import Data.Array (null)
import Data.Array as Array
import Data.Maybe (Maybe(..), fromMaybe)
import Effect (Effect)
import Effect.Random (randomInt)
import Debug (spy)
import Data.List (List, (:))
import Data.Function (applyN)
import Data.Foldable (foldl)

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

foreign import binLowest :: forall a b . NonEmptyArray a -> Array Number -> (a -> b) -> NonEmptyArray (Array a)

foreign import partialSort :: forall a b . NonEmptyArray a -> Int -> (a -> b) -> NonEmptyArray {x :: a, y :: b}

binnedLowest :: NonEmptyArray Flashcard -> NonEmptyArray (Array {x :: Flashcard, y :: Number})
binnedLowest flashcards = fromMaybe bins $ fromArray $ foldl (\accum bin -> if null bin then accum else Array.snoc accum bin) mempty bins
  where bins = binLowest (sortedItems flashcards) [1e-3, 1e-2, 1e-1, 5e-1] (\e -> e.y)

randomLow :: NonEmptyArray Flashcard -> Effect (Maybe {x :: Flashcard, y :: Number})
randomLow flashcards = randomLowest $ binnedLowest flashcards
  where randomLowest l = do
          let lowestBin = fromMaybe [] $ l !! 0
          index <- randomInt 0 (Array.length lowestBin - 1)
          pure $ Array.index lowestBin index

nRandom :: Int -> NonEmptyArray Flashcard -> Effect (List {x :: Flashcard, y :: Number})
nRandom n flashcards = do
    randomValues <- applyN getRandom (min n (NonEmpty.length flashcards)) (pure {bins: binnedLowest flashcards, result: mempty})
    pure $ randomValues.result

  where getRandom previousPayload = do
          {bins, result: previousResults} <- previousPayload
          {result: newItem, bins: newBins} <- takeRandomLowest bins
          case newItem of
               Just item -> pure $ {bins: newBins, result: item : previousResults}
               Nothing -> pure $ {bins: newBins, result: previousResults}

        takeRandomLowest l = do
          let lowestBin = fromMaybe [] $ l !! 0
          index <- randomInt 0 (Array.length lowestBin - 1)
          let newBins = do
                newLowest <- Array.deleteAt index lowestBin
                if null newLowest
                  then do
                     maybeEmpty <- NonEmpty.deleteAt 0 l
                     fromArray maybeEmpty
                  else updateAt 0 newLowest l
          pure $ spy "RESULT" {result: Array.index lowestBin index, bins: fromMaybe l newBins}

lowest :: NonEmptyArray Flashcard -> Array {x :: Flashcard, y :: Number}
lowest flashcards = toArray $ spy "SORTED FLASHCARDS" $ sortedItems flashcards

sortedItems :: NonEmptyArray Flashcard -> NonEmptyArray {x :: Flashcard, y :: Number}
sortedItems flashcards = partialSort (spy "SORTED" flashcards) numberOfItems sortFn
  where sortFn flashcard = predictRecall (flashcard.a /\ flashcard.b /\ flashcard.t) $ spy "LAST REVIEWED" flashcard.hoursPassed
        numberOfItems = min 30 (NonEmpty.length flashcards)
