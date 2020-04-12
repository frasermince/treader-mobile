module KeyboardAwareDialog where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _keyboardAwareDialog :: forall props. ReactComponent props

keyboardAwareDialog = parentElement _keyboardAwareDialog
