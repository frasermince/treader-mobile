module EpubRn where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect (Effect)

foreign import epub :: forall props . ReactComponent props
foreign import _startStream :: Effect (Promise String)
foreign import _streamGet :: String -> Effect (Promise String)
foreign import killStream :: Effect Unit

startStream :: Aff String
startStream = liftEffect _startStream >>= Promise.toAff

streamGet :: String -> Aff String
streamGet x = liftEffect (_streamGet x) >>= Promise.toAff

