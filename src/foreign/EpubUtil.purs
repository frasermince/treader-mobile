module EpubUtil where

import Prelude
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, EffectFn1, runEffectFn1, EffectFn2, runEffectFn2)
import Effect.Aff (Aff)
import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic.Hooks ((/\))
import Data.Function.Uncurried (Fn1, mkFn1, runFn1)
import React.Basic.Hooks (ReactComponent)
import Data.Tuple (Tuple)
import Data.Nullable (toMaybe, toNullable, Nullable)
import Data.Tuple.Native (T2, t2)
import Debug.Trace (spy)

foreign import bridgeFile :: String

foreign import epubjs :: String

--foreign import webview :: forall props . ReactComponent props
type HighlightedContent
  = { text :: String, fromTop :: Number }

type StateChangeFn a
  = EffectFn1 (Fn1 a a) Unit

type StateChangeTuple a
  = T2 a (StateChangeFn a)

type MaybeStateChangeTuple a
  = T2 (Nullable a) (EffectFn1 (Fn1 (Nullable a) (Nullable a)) Unit)

mkStateChangeTuple :: forall a. Tuple a ((a -> a) -> Effect Unit) -> StateChangeTuple a
mkStateChangeTuple a = t2 value (mkStateChangeFn fn)
  where
  (value /\ fn) = a

mkStateChangeFn :: forall a. ((a -> a) -> Effect Unit) -> StateChangeFn a
mkStateChangeFn fn = mkEffectFn1 $ paramFn fn
  where
  paramFn fn x = mkFn1 fn x

mkMaybeStateChangeTuple :: forall a. (Tuple (Maybe a) ((Maybe a -> Maybe a) -> Effect Unit)) -> MaybeStateChangeTuple a
mkMaybeStateChangeTuple (value /\ fn) = t2 (toNullable value) (mkEffectFn1 $ effectFn fn)
  where
  effectFn :: ((Maybe a -> Maybe a) -> Effect Unit) -> (Fn1 (Nullable a) (Nullable a)) -> Effect Unit
  effectFn fn stateChange = fn $ paramFn stateChange

  paramFn :: (Fn1 (Nullable a) (Nullable a)) -> Maybe a -> Maybe a
  paramFn fn x = toMaybe $ runFn1 fn $ toNullable x

type StateChangeListeners
  = { translation :: MaybeStateChangeTuple String
    , highlightedContent :: MaybeStateChangeTuple HighlightedContent
    , epubcfi :: MaybeStateChangeTuple String
    , morphology :: MaybeStateChangeTuple {}
    , language :: MaybeStateChangeTuple String
    , chapterTitle :: MaybeStateChangeTuple String
    }

type BridgeData x y
  = { mutationFn :: Record (x) -> Aff (Record y)
    }

mkStateChangeListeners rd =
  rd
    { translation = spy "translation" $ mkMaybeStateChangeTuple $ rd.translation
    , highlightedContent = mkMaybeStateChangeTuple rd.highlightedContent
    , epubcfi = mkMaybeStateChangeTuple rd.epubcfi
    , morphology = mkMaybeStateChangeTuple rd.morphology
    , language = mkMaybeStateChangeTuple rd.language
    , chapterTitle = mkMaybeStateChangeTuple rd.chapterTitle
    , sentence = mkMaybeStateChangeTuple rd.sentence
    , phrase = mkMaybeStateChangeTuple rd.phrase
    }
