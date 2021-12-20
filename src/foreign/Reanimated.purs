module Reanimated where

import Prelude (Unit)
import Prim.Row (class Union)
import React.Basic.Hooks ( Hook, unsafeHook, UseEffect, UseContext)
import Effect (Effect)
import Data.Tuple.Nested (type (/\), (/\))

type Reanimation
  = {}

type AnimationXY
  = { x :: Number, y :: Number }

type InterpolateOptions a
  = ( inputRange :: Array Number, outputRange :: Array a, extrapolate :: String, easing :: Number -> a )

foreign import interpolate ::
  forall opts _opts a.
  (Union opts _opts (InterpolateOptions a)) =>
  Number ->
  Record opts ->
  Number

foreign import data UseSharedValue :: Type -> Type -> Type

foreign import useSharedValue :: forall a . a -> Hook (UseSharedValue a) (a /\ ((a -> a) -> Effect Unit))

foreign import useAnimatedStyle :: forall a . a -> a

