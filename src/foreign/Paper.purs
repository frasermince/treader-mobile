module Paper where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _textInput :: forall props. ReactComponent props

foreign import _surface :: forall props. ReactComponent props

foreign import _button :: forall props. ReactComponent props

foreign import _listSection :: forall props. ReactComponent props

foreign import _listItem :: forall props. ReactComponent props

foreign import listIcon :: forall props. ReactComponent props

foreign import _title :: forall props. ReactComponent props

foreign import _menu :: forall props. ReactComponent props
foreign import _menuItem :: forall props. ReactComponent props
foreign import _switch :: forall props. ReactComponent props

foreign import navigationOptions ::
  forall props opts.
  ReactComponent props -> Record opts -> ReactComponent props

textInput = childElement _textInput

surface = parentElement _surface

button = parentElement _button

listSection = parentElement _listSection

listItem = childElement _listItem

switch = childElement _switch

title = parentElement _title

menu = parentElement _menu
menuItem = childElement _menuItem
