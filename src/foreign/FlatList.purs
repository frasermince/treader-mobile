module FlatList where
import React.Basic.Hooks (ReactComponent)
import Markup
foreign import _flatList :: forall props. ReactComponent props

flatList = childElement _flatList
