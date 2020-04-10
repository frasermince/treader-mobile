module TextToSpeech where
import Prelude
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)

foreign import _speak :: forall a . EffectFn2 String (Record a) Unit
foreign import _setDefaultLanguage :: forall a . EffectFn1 String Unit

speak = runEffectFn2 _speak
setDefaultLanguage = runEffectFn1 _setDefaultLanguage
