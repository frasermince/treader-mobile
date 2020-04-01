module Linking where

import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
foreign import _openUrl :: EffectFn1 String Unit

openUrl :: String -> Effect Unit
openUrl = runEffectFn1 $ _openUrl

foreign import canOpenUrl :: String -> Boolean
