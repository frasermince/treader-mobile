module MaterialIcon where

import Markup
import React.Basic.Hooks (ReactComponent)

foreign import _icon :: forall props. ReactComponent props

icon = childElement _icon
