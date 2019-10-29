module Animated where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type Animation = {}

foreign import value :: Int -> Animation
foreign import view :: forall attrs. ReactComponent (Record attrs)
foreign import _timing :: Animation -> {toValue :: Int, duration :: Int} -> EffectFnAff Unit
timing :: Animation -> {toValue :: Int, duration :: Int} -> Aff Unit
timing x y = fromEffectFnAff $ (_timing x y)
