module StackSwiper where
import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _cardStack :: forall props. ReactComponent props
foreign import _card :: forall props. ReactComponent props

cardStack = parentElement _cardStack
card = parentElement _card
