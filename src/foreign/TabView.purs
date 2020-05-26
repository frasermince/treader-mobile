module TabView where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

data SceneMap = SceneMap

foreign import _tabView :: forall props. ReactComponent props
foreign import _tabBar :: forall props. ReactComponent props
foreign import sceneMap :: forall a props. Record a -> SceneMap

tabView = childElement _tabView
tabBar = childElement _tabBar
