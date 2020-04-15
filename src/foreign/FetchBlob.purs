module FetchBlob where
import Prelude
import Effect.Aff (Aff)
import Effect (Effect)
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)


foreign import _fetch :: forall a b. String -> String -> Record a -> Effect (Promise (Record b))


fetch :: forall a b . String -> String -> Record a -> Aff (Record b)
fetch method url options = (liftEffect (_fetch method url options) >>= Promise.toAff)
