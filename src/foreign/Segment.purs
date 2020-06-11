module Segment where

import Prelude
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Effect (Effect)
import Effect.Aff (Aff, forkAff, Fiber)
import Effect.Class (liftEffect)

foreign import _identify :: String -> {email :: String} -> Effect Unit

foreign import _track :: forall a . String -> Record a -> Effect Unit

foreign import _screen :: forall a . String -> Record a -> Effect Unit

identify :: forall a . String -> {email :: String} -> Aff (Fiber Unit)
identify id d = forkAff $ liftEffect $ _identify id d

track :: forall a . String -> Record a -> Aff (Fiber Unit)
track event d = forkAff $ liftEffect $ _track event d

screen :: forall a . String -> Record a -> Aff (Fiber Unit)
screen event d = forkAff $ liftEffect $ _screen event d
