module Animated where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)
import Markup
import Web.DOM.Internal.Types (Node)
import Prim.Row (class Union)


type Animation
  = {}

type AnimationXY
  = { x :: Animation, y :: Animation }

foreign import value :: Number -> Animation

foreign import _view :: forall attrs. ReactComponent (Record attrs)

foreign import _scrollView :: forall attrs. ReactComponent (Record attrs)

foreign import _timing :: Animation -> { toValue :: Int, duration :: Int } -> EffectFnAff Unit

foreign import getNode :: forall opts. Node -> Node

foreign import scrollTo :: Node -> Int -> Effect Unit

type InterpolateOptions a
  = ( inputRange :: Array Number, outputRange :: Array a, extrapolate :: String, easing :: Number -> a )

foreign import interpolate ::
  forall opts _opts a.
  (Union opts _opts (InterpolateOptions a)) =>
  Animation ->
  Record opts ->
  Number

timing :: Animation -> { toValue :: Int, duration :: Int } -> Aff Unit
timing x y = fromEffectFnAff $ (_timing x y)

view = parentElement _view

scrollView = parentElement _scrollView
