module BottomBar where

import Prelude
import Debug.Trace (spy)
import Effect.Aff (Aff, launchAff_)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import React.Basic.Hooks as React
import React.Basic.Native as RN
import Animated (view, timing, value)
import Effect.Unsafe (unsafePerformEffect)
import Record as Record
import Platform as Platform
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Slider (slider)
import Effect.Uncurried (EffectFn1, mkEffectFn1)

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "BottomBar") buildJsx


type Props = { shown :: Boolean, disabled :: Boolean, value :: Int, onSlidingComplete :: Number -> Effect Unit}
styles = {
  footer: {
    backgroundColor: "#cdcdcd",
    paddingTop: 0,
    bottom: 0,
    height: Platform.select {ios: 64, android: 54},
    right: 0,
    left: 0,
    borderTopWidth: 1,
    borderTopColor:"#000",
    position: "absolute",
    alignItems:"center",
    justifyContent:"center",
    flexDirection: "row"
  },
  slider: {
    height: 30,
    alignItems:"center",
    justifyContent:"center",
    flexDirection: "row",
    flex: 1,
    marginLeft: 50,
    marginRight: 50
  }
}

runAnimation true fade = timing fade {toValue: 1, duration: 20}
runAnimation false fade = timing fade {toValue: 0, duration: 20}
opacity = SProxy :: SProxy "opacity"
--buildJsx :: Props -> JSX
buildJsx props = React.do
  fade /\ setFade <- useState $ value 1
  useEffect props.shown do
     launchAff_ $ runAnimation props.shown fade
     pure mempty

  pure $ element view {style: Record.insert opacity fade styles.footer, children: [
    element slider {style: styles.slider, disabled: props.disabled, value: props.value, onSlidingComplete: mkEffectFn1 props.onSlidingComplete}
  ]}
