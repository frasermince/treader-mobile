module Sound where
import Prelude
import Effect (Effect)
import Effect.Aff (Aff)
import Effect.Aff.Compat (EffectFnAff, fromEffectFnAff)

type Sound = {}
foreign import _createSound :: String -> EffectFnAff Sound
foreign import _play :: Sound -> EffectFnAff Unit
foreign import _stop :: Sound -> EffectFnAff Sound
foreign import release :: Sound -> Effect Unit

createSound :: String -> Aff Sound
createSound = fromEffectFnAff <<< _createSound

stop :: Sound -> Aff Sound
stop = fromEffectFnAff <<< _stop

play :: Sound -> Aff Unit
play = fromEffectFnAff <<< _play

stopAndPlay :: Sound -> Aff Unit
stopAndPlay sound = do
  stoppedSound <- stop sound
  play stoppedSound

