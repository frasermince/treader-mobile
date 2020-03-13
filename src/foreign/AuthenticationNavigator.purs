module AuthenticationNavigator where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _authenticationNavigator :: forall props. ReactComponent props

authenticationNavigator = childElement _authenticationNavigator
