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

type InterpolateOptions
  = ( inputRange :: Array Number, outputRange :: Array Number, extrapolate :: String )

foreign import interpolate ::
  forall opts _opts.
  (Union opts _opts InterpolateOptions) =>
  Animation ->
  Record opts ->
  Number

timing :: Animation -> { toValue :: Int, duration :: Int } -> Aff Unit
timing x y = fromEffectFnAff $ (_timing x y)

view = parentElement _view

scrollView = parentElement _scrollView
