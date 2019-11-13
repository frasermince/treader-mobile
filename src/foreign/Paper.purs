module Paper where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _textInput :: forall props . ReactComponent props
foreign import _surface :: forall props . ReactComponent props
foreign import _button :: forall props . ReactComponent props

textInput = childElement _textInput
surface = parentElement _surface
button = parentElement _button
