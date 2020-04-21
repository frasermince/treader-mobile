module FetchBlob where
import Prelude
import Effect.Aff (Aff)
import Effect (Effect)
import Control.Promise as Promise
import Control.Promise (Promise)
import Effect.Class (liftEffect)


foreign import _fetch :: forall a b c. Record a -> String -> String -> Record b -> Effect (Promise (Record c))


fetch :: forall a b c . Record a -> String -> String -> Record b -> Aff (Record c)
fetch config method url options = (liftEffect (_fetch config method url options) >>= Promise.toAff)
