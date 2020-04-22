module Sound where
import Prelude
import Effect (Effect)

type Sound = {}
foreign import createSound :: String -> Sound
foreign import play :: Sound -> Effect Unit
foreign import stop :: Sound -> Effect Unit -> Effect Unit
foreign import release :: Sound -> Effect Unit
