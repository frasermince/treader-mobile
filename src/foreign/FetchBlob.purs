module FetchBlob where
import Prelude
import Effect.Aff (Aff)
import Effect (Effect)
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, mkEffectFn2, runEffectFn2, EffectFn2)

type FetchResult = {path :: Effect String}
foreign import _fetch :: forall a b c. Record a -> String -> String -> Record b -> Effect (Promise FetchResult)
foreign import _progress :: forall a. Promise a -> (EffectFn2 Number Number Unit) -> Effect (Promise Unit)

fetch :: forall a b c . Record a -> String -> String -> Record b -> Aff FetchResult
fetch config method url options = liftEffect (_fetch config method url options) >>= Promise.toAff

fetchWithProgress :: forall a b c . Record a -> String -> String -> Record b -> (Number -> Number -> Effect Unit) -> Aff FetchResult
fetchWithProgress config method url options onProgress = do
  let request = (_fetch config method url options)
  liftEffect (request >>= (\r -> _progress r $ mkEffectFn2 onProgress)) >>= Promise.toAff
  liftEffect request >>= Promise.toAff

