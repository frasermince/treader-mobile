module Blur where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Effect (Effect)
import Markup

foreign import _blurView :: forall props. ReactComponent props

blurView = childElement _blurView
