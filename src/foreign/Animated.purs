module Animated where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Markup

type Animation
  = { interpolate :: { inputRange :: Array Int, outputRange :: Array Int } -> Int }

foreign import value :: Int -> Animation

foreign import _view :: forall attrs. ReactComponent (Record attrs)

foreign import _scrollView :: forall attrs. ReactComponent (Record attrs)

foreign import _timing :: Animation -> { toValue :: Int, duration :: Int } -> EffectFnAff Unit

timing :: Animation -> { toValue :: Int, duration :: Int } -> Aff Unit
timing x y = fromEffectFnAff $ (_timing x y)

view = parentElement _view

scrollView = parentElement _scrollView
