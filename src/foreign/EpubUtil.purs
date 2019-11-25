module EpubUtil where

import Prelude
import Effect (Effect)
import Effect.Uncurried (mkEffectFn1, EffectFn1, runEffectFn1)
import Effect.Aff (Aff)
import Data.Function.Uncurried (Fn1, mkFn1)
foreign import _renditionHandler :: forall x y . EffectFn1 (RenditionData x y) Unit

type StateChangeFn a = EffectFn1 (Fn1 a a) Unit

createStateChangeFn :: forall a . ((a -> a) -> Effect Unit) -> StateChangeFn a
createStateChangeFn fn = mkEffectFn1 $ paramFn fn
  where paramFn fn x = mkFn1 fn x
type RenditionData x y=  {
  mutationFn ::  Record (x) -> Aff (Record y),
  setHighlightedContent :: StateChangeFn String,
  setSnippet :: StateChangeFn String,
  setEpubcfi :: StateChangeFn String,
  setWordData :: StateChangeFn String,
  language :: String,
  setLanguage :: StateChangeFn String,
  highlightVerbs :: Boolean,
  highlightNouns :: Boolean,
  highlightAdjectives :: Boolean,
  setHighlightedVerbs :: StateChangeFn Boolean,
  setHighlightedNouns :: StateChangeFn Boolean,
  setHighlightedAdjectives :: StateChangeFn Boolean,
  setChapterTitle :: StateChangeFn String,
  chaptherTitle :: String,
  location :: String
 }

renditionHandler :: forall x y . RenditionData x y -> Effect Unit
renditionHandler r = runEffectFn1 _renditionHandler r
