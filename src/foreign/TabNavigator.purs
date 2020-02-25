module TabNavigator where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _tabNavigator :: forall props. ReactComponent props

tabNavigator = childElement _tabNavigator
