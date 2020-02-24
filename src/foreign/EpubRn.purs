module EpubRn where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Effect.Class (liftEffect)
import Effect.Aff (Aff)
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect (Effect)
import Effect.Uncurried (runEffectFn1, EffectFn1, EffectFn2, runEffectFn2)
import Data.Function.Uncurried (Fn0, runFn0)

type Streamer
  = {}

foreign import epub :: forall props. ReactComponent props

foreign import _createStreamer :: Fn0 Streamer

foreign import _startStream :: Streamer -> Effect (Promise String)

foreign import _streamGet :: Streamer -> String -> Effect (Promise String)

foreign import _killStream :: EffectFn1 Streamer Unit

createStreamer :: Streamer
createStreamer = runFn0 _createStreamer

startStream :: Streamer -> Aff String
startStream x = (liftEffect (_startStream x) >>= Promise.toAff)

killStream :: Streamer -> Effect Unit
killStream = runEffectFn1 _killStream

streamGet :: Streamer -> String -> Aff String
streamGet stream string = liftEffect (_streamGet stream string) >>= Promise.toAff
