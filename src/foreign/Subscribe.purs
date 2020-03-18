module Foreign.Subscribe where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _subscribe :: forall props. ReactComponent props

subscribe = childElement _subscribe
