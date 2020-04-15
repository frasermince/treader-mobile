module Sound where
import Prelude
import Effect (Effect)

foreign import play :: String -> Effect Unit
