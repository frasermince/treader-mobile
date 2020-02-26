module HTMLView where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _htmlView :: forall props. ReactComponent props

htmlView = childElement _htmlView
