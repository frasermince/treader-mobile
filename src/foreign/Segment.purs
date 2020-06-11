module Segment where

import Prelude
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Effect (Effect)

foreign import identify :: String -> {email :: String} -> Effect Unit

foreign import track :: forall a . String -> Record a -> Effect Unit

foreign import screen :: forall a . String -> Record a -> Effect Unit
