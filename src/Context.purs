module Context where
import Prelude
import Effect (Effect)
import React.Basic.Hooks (unsafeRenderEffect, createContext, ReactComponent, JSX, contextProvider, UseContext, ReactContext, Pure)
import Effect.Unsafe (unsafePerformEffect)

type Context = {setLoading :: (Boolean -> Boolean) -> Effect Unit, setError :: (String -> String) -> Effect Unit}
renderContext :: Pure (ReactContext Context)
renderContext = unsafeRenderEffect dataStateContext

dataStateContext :: Effect (ReactContext Context)
dataStateContext = createContext {setLoading: \_ -> pure unit, setError: \_ -> pure unit}

provider :: ReactComponent { children :: Array JSX, value :: Context }
provider = unsafePerformEffect $ do
  context <- dataStateContext
  pure $ contextProvider context
