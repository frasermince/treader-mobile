module Reanimated where

import Prelude

import Data.Function.Uncurried (Fn2, mkFn2, runFn2)
import Data.Tuple (Tuple(..))
import Data.Tuple.Native (T2, t2, prj)
import Data.Tuple.Nested (type (/\), (/\))
import Data.Typelevel.Num.Reps (d0, d1, d2)
import Debug (spy)
import Effect (Effect)
import Effect.Uncurried (EffectFn2, runEffectFn2, runEffectFn3, EffectFn3)
import Prim.Row (class Union)
import React.Basic.Hooks (Hook, unsafeHook, UseEffect, UseContext, (/\), type (/\))
import React.Basic.Hooks as React
import Markup
import React.Basic.Hooks (ReactComponent)
type Reanimation
  = {}

type AnimationXY
  = { x :: Number, y :: Number }

type InterpolateOptions a
  = ( inputRange :: Array Number, outputRange :: Array a, extrapolate :: String, easing :: Number -> a )

foreign import interpolate ::
  Number ->
  Array Number ->
  Array Number ->
  String ->
  Number

foreign import data UseSharedValue :: Type -> Type -> Type
foreign import data UseAnimatedStyle :: Type -> Type -> Type

-- foreign import _useSharedValue :: forall a . a -> Hook (UseSharedValue a) (T2 a (EffectFn1 (a -> a) Unit))
foreign import _useSharedValue :: forall state.
    EffectFn2
    (forall a b. Fn2 a b (a /\ b))
    state
    ({value :: state} /\ ((state -> Effect Unit)))

-- useSharedValue :: forall a . a -> Hook (UseSharedValue a) (a /\ ((a -> a) -> Effect Unit))
-- useSharedValue a =  React.do
--   result <- spy "***VALUE" $ _useSharedValue $ spy "INNER VALUE" a
--   pure $ (spy "TUPLE0" $ prj d0 result) /\ (spy "TUPLE1" $ runEffectFn1 $ prj d1 result)

useSharedValue :: forall a . a -> Hook (UseSharedValue a) ({value :: a} /\ ((a -> Effect Unit)))
useSharedValue initialState = unsafeHook do
    runEffectFn2 _useSharedValue (mkFn2 Tuple) initialState

useAnimatedStyle :: forall a deps . Eq deps => deps -> Effect a -> Hook (UseAnimatedStyle a) a
useAnimatedStyle deps a = unsafeHook do
  runEffectFn3 _useAnimatedStyle (mkFn2 eq) deps a

foreign import _useAnimatedStyle :: forall a deps . EffectFn3 (Fn2 deps deps Boolean) deps (Effect a) (a)


foreign import _view :: forall attrs .  ReactComponent (Record attrs)

view = parentElement _view
