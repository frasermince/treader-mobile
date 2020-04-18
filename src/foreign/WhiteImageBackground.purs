module WhiteImageBackground where
import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup


foreign import _whiteImageBackground :: forall props. ReactComponent props

whiteImageBackground = parentElement _whiteImageBackground
