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

foreign import _iconButton :: forall props. ReactComponent props

foreign import _menu :: forall props. ReactComponent props

foreign import _menuItem :: forall props. ReactComponent props

foreign import _divider :: forall props. ReactComponent props

foreign import _modal :: forall props. ReactComponent props
foreign import _portal :: forall props. ReactComponent props
foreign import _subheading :: forall props. ReactComponent props
foreign import _headline :: forall props. ReactComponent props
foreign import _paragraph :: forall props. ReactComponent props
foreign import _badge :: forall props. ReactComponent props
foreign import _fab :: forall props. ReactComponent props
foreign import _dialog :: forall props. ReactComponent props
foreign import _dialogTitle :: forall props. ReactComponent props
foreign import _dialogContent :: forall props. ReactComponent props
foreign import _dialogActions :: forall props. ReactComponent props
foreign import _searchbar :: forall props. ReactComponent props

foreign import navigationOptions ::
  forall props opts.
  ReactComponent props -> Record opts -> ReactComponent props

textInput = childElement _textInput

surface = parentElement _surface

button = parentElement _button

listSection = parentElement _listSection

listItem = childElement _listItem

iconButton = childElement _iconButton

title = parentElement _title

menu = parentElement _menu

modal = parentElement _modal

menuItem = childElement _menuItem

divider = childElement _divider

paragraph = parentElement _paragraph

portal = parentElement _portal

subheading = parentElement _subheading

headline = parentElement _headline

badge = parentElement _badge

fab = childElement _fab

searchbar = childElement _searchbar

dialog = parentElement _dialog
dialogContent = parentElement _dialogContent
dialogActions = parentElement _dialogActions
dialogTitle = parentElement _dialogTitle
