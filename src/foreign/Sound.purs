module Sound where
import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type Sound = {}
foreign import _createSound :: String -> EffectFnAff Sound
foreign import _play :: Sound -> EffectFnAff Unit
foreign import _pause :: Sound -> EffectFnAff Unit
foreign import _stop :: Sound -> EffectFnAff Sound
foreign import _setCurrentTime :: Sound -> Number -> EffectFnAff Sound
foreign import _getCurrentTime :: Sound -> EffectFnAff Number
foreign import release :: Sound -> Effect Unit

createSound :: String -> Aff Sound
createSound = fromEffectFnAff <<< _createSound

stop :: Sound -> Aff Sound
stop = fromEffectFnAff <<< _stop

setCurrentTime :: Sound -> Number -> Aff Sound
setCurrentTime sound timestamp = fromEffectFnAff $ _setCurrentTime sound timestamp

getCurrentTime :: Sound -> Aff Number
getCurrentTime sound = fromEffectFnAff $ _getCurrentTime sound

play :: Sound -> Aff Unit
play = fromEffectFnAff <<< _play

pause :: Sound -> Aff Unit
pause = fromEffectFnAff <<< _pause

stopAndPlay :: Sound -> Aff Unit
stopAndPlay sound = do
  stoppedSound <- stop sound
  play stoppedSound

