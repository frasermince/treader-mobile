module Image where
import React.Basic.Hooks (ReactComponent)
import Markup

data ResizeMode = ResizeMode
foreign import _image :: forall props. ReactComponent props
foreign import contain :: ResizeMode
foreign import cover :: ResizeMode

image = childElement _image
