module Ebisu where
import Prelude
import Data.Tuple.Native (T3, t3, prj)
import React.Basic.Hooks ((/\), type(/\))
import Data.Typelevel.Num.Reps (d0, d1, d2)

type EbisuModelJs = T3 Number Number Number
type EbisuModel = Number /\ Number /\ Number
foreign import _defaultModel :: Number -> EbisuModelJs
foreign import _predictRecall :: EbisuModelJs -> Number -> Number
foreign import _updateRecall :: EbisuModelJs -> Boolean -> Number -> EbisuModelJs

toTuple :: EbisuModelJs -> EbisuModel
toTuple tuple = (prj d0 tuple) /\ (prj d1 tuple) /\ (prj d2 tuple)

defaultModel :: Number -> EbisuModel
defaultModel n = toTuple $ _defaultModel n

predictRecall :: EbisuModel -> Number -> Number
predictRecall (a /\ b /\ t) elapsed = _predictRecall (t3 a b t) elapsed

updateRecall :: EbisuModel -> Boolean -> Number -> EbisuModel
updateRecall (a /\ b /\ t) result elapsed = toTuple $ _updateRecall (t3 a b t) result elapsed
