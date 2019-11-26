module EpubUtil where

import Prelude
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, EffectFn1, runEffectFn1)
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic.Hooks ((/\))
import Data.Function.Uncurried (Fn1, mkFn1)
import Data.Tuple (Tuple)
foreign import _renditionHandler :: forall x y . EffectFn1 (RenditionData x y) Unit

type StateChangeFn a = EffectFn1 (Fn1 a a) Unit

type StateChangeTuple a = Tuple a (StateChangeFn a)

mkStateChangeTuple :: forall a . Tuple a ((a -> a) -> Effect Unit) -> StateChangeTuple a
mkStateChangeTuple a = value /\ mkStateChangeFn fn
  where (value /\ fn) = a
mkStateChangeFn :: forall a . ((a -> a) -> Effect Unit) -> StateChangeFn a
mkStateChangeFn fn = mkEffectFn1 $ paramFn fn
  where paramFn fn x = mkFn1 fn x
type RenditionData x y =  {
  mutationFn ::  Record (x) -> Aff (Record y),
  translation :: StateChangeTuple (Maybe String),
  highlightedContent :: StateChangeTuple (Maybe String),
  epubcfi :: StateChangeTuple (Maybe String),
  morphology :: StateChangeTuple (Maybe {}),
  language :: StateChangeTuple (Maybe String),
  highlightedVerbs :: StateChangeTuple Boolean,
  highlightedNouns :: StateChangeTuple Boolean,
  highlightedAdjectives :: StateChangeTuple Boolean,
  chapterTitle :: StateChangeTuple (Maybe String),
  location :: String
 }

mkRenditionData rd = rd
  { translation = mkStateChangeTuple $ rd.translation
  , highlightedContent = mkStateChangeTuple rd.highlightedContent
  , epubcfi = mkStateChangeTuple rd.epubcfi
  , morphology = mkStateChangeTuple rd.morphology
  , language = mkStateChangeTuple rd.language
  , highlightedVerbs = mkStateChangeTuple rd.highlightedVerbs
  , highlightedNouns = mkStateChangeTuple rd.highlightedNouns
  , highlightedAdjectives = mkStateChangeTuple rd.highlightedAdjectives
  , chapterTitle = mkStateChangeTuple rd.chapterTitle
  }

renditionHandler :: forall x y . RenditionData x y -> Effect Unit
renditionHandler r = runEffectFn1 _renditionHandler r
