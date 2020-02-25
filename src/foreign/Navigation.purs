module Navigation where
import Prelude
import Data.Function.Uncurried (Fn2, mkFn2)
import Effect.Uncurried (EffectFn3, runEffectFn3)
import Effect (Effect)
import React.Basic.Hooks ( Hook, unsafeHook, UseEffect)

foreign import useFocusEffect_ ::
  forall key.
  EffectFn3
    (Fn2 key key Boolean)
    key
    (Effect (Effect Unit))
    Unit
useFocusEffect :: forall key.
  Eq key =>
  key ->
  Effect (Effect Unit) ->
  Hook (UseEffect key) Unit
useFocusEffect key effect = unsafeHook (runEffectFn3 useFocusEffect_ (mkFn2 eq) key effect)
