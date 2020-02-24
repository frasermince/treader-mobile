module Context where

import Prelude
import Effect (Effect)
import React.Basic.Hooks (unsafeRenderEffect, createContext, ReactComponent, JSX, contextProvider, UseContext, ReactContext, Pure)
import Effect.Unsafe (unsafePerformEffect)
import Data.Lazy (Lazy, defer, force)
import Effect.Uncurried (EffectFn1)

type Context
  = { setLoading :: EffectFn1 (Boolean -> Boolean) Unit, setError :: EffectFn1 String Unit }

foreign import dataStateContext :: (ReactContext Context)

provider :: ReactComponent { children :: Array JSX, value :: Context }
provider =
  unsafePerformEffect
    $ do
        pure $ contextProvider dataStateContext
