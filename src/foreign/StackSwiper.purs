module StackSwiper where
import Prelude
import React.Basic.Hooks (ReactComponent)
import Debug (spy)
import Markup

foreign import _cardStack :: forall props. ReactComponent props
foreign import _card :: forall props. ReactComponent props

cardStack = parentWithCallbacks $ spy "CARD" _cardStack
card = parentElement _card
