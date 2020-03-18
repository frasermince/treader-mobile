module Swiper where

import Prelude
import React.Basic.Hooks (ReactComponent)
import Markup

foreign import _swiper :: forall props. ReactComponent props

swiper = parentElement _swiper
