module TopBar where
import Prelude
import Platform as Platform
import Effect.Aff (Aff, launchAff_)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import Unsafe.Coerce (unsafeCoerce)
import React.Basic.Hooks as React
import React.Basic.Native as RN
import Animated (view, timing, value)
import Effect.Unsafe (unsafePerformEffect)
import Record as Record
import Platform as Platform
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Icon (icon)
import Effect.Uncurried (EffectFn1)
import React.Basic.Native.Events (NativeSyntheticEvent)

css :: forall css. { | css } -> CSS
css = unsafeCoerce
type Props = { shown :: Boolean, onLeftButtonPressed :: EffectFn1 (NativeSyntheticEvent RN.NativeTouchEvent) Unit, onRightButtonPressed :: EffectFn1 (NativeSyntheticEvent RN.NativeTouchEvent) Unit, title :: String}
styles = {
  title: {
    textAlign: "center",
    fontSize: 22,
    fontWeight: "400",
    flex: 8,
    color: "#000",
    fontFamily: Platform.select {
      ios: "Baskerville",
      android: "serif"
    }
  },
  header: {
    backgroundColor: "#cdcdcd",
    paddingTop: Platform.select {ios: 40, android: 24},
    top: 0,
    height: Platform.select {
      ios: 84,
      android: 74
    },
    right: 0,
    left: 0,
    borderBottomWidth: 1,
    borderBottomColor:"#000",
    position: "absolute",
    display: "flex",
    alignItems:"center",
    justifyContent:"center",
    flexDirection: "row",
    flex: 14
  },
  backButton: {
    width: 34,
    height: 34,
    margin: 20,
    flex: 1,
    display: "flex",
    alignItems:"center",
    justifyContent:"center",
    flexDirection: "row"
  },
  backButtonImage: {
    width: 30,
    height: 30
  }
}

runAnimation true fade = timing fade {toValue: 1, duration: 20}
runAnimation false fade = timing fade {toValue: 0, duration: 20}
opacity = SProxy :: SProxy "opacity"

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "TopBar") buildJsx

buildJsx props = React.do
  fade /\ setFade <- useState $ value 1
  useEffect props.shown do
     launchAff_ $ runAnimation props.shown fade
     pure mempty
  pure $ element view {style: Record.insert opacity fade styles.header, children: children props }

children props = [
  RN.touchableOpacity {style: css styles.backButton, onPress: props.onLeftButtonPressed, children: [element icon {name: "navicon", size: 34}]},
  RN.text {style: css styles.title, children: [RN.string props.title]},
  RN.touchableOpacity {style: css styles.backButton, onPress: props.onRightButtonPressed, children: [element icon {name: "gear", size: 34}]}
]
  

