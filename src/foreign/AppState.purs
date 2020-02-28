module AppState where
import Prelude
import Effect (Effect)
import Effect.Uncurried (EffectFn1, runEffectFn1)
import React.Basic.Hooks.Internal (unsafeHook)
import React.Basic.Hooks (Hook, UseEffect)

type AppStateHandlers = {onForeground :: Effect Unit}
foreign import _useAppState :: EffectFn1 AppStateHandlers Unit

useAppState :: AppStateHandlers -> Hook (UseEffect Unit) Unit

useAppState handlers = unsafeHook $ runEffectFn1 _useAppState handlers

