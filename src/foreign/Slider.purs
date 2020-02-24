module Slider where

import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _slider :: forall props. ReactComponent props

slider = childElement _slider
