module Copilot where

import Prelude
import React.Basic.Hooks (ReactComponent, component, element, JSX)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Native as RN
import Markup as M
import Prim.Row (class Cons, class Lacks)
import Markup
import Debug.Trace (spy)
import Record (delete)
import Data.Symbol (SProxy(..))

foreign import _walkthroughable :: forall p . ReactComponent (Record p) -> ReactComponent (Record p)

foreign import _copilotStep :: forall props. ReactComponent props

foreign import _copilot :: forall p opts . Record opts -> ReactComponent p -> ReactComponent p

copilot opts c = parentElement $ _copilot opts c
copilotStep = parentElement _copilotStep
--walkthroughableParent c = parentElement $ _walkthroughable $ wrappedChild $ parentElement c
walkthroughableChild c = childElement $ _walkthroughable c
walkthroughableParent c = parentElement $ _walkthroughable c
