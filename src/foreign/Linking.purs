module Linking where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import Debug.Trace (spy)
foreign import _openUrl :: EffectFn1 String Unit

openUrl :: String -> Effect Unit
openUrl = runEffectFn1 $ spy "OPEN" _openUrl
