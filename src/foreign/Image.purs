module Image where
import React.Basic.Hooks (ReactComponent)
import Markup
foreign import _image :: forall props. ReactComponent props

image = childElement _image
