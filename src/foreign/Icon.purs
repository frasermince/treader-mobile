module Icon where

import React.Basic.Hooks (ReactComponent)
import Markup

foreign import evilIcon :: forall props. ReactComponent props

foreign import _fontAwesomeIcon :: forall props. ReactComponent props

foreign import _materialIcon :: forall props. ReactComponent props
foreign import _materialCommunityIcon :: forall props. ReactComponent props

materialIcon = childElement _materialIcon
materialCommunityIcon = childElement _materialCommunityIcon
fontAwesomeIcon = childElement _fontAwesomeIcon
