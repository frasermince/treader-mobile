module TopBar where
import Prelude
import Platform as Platform
import Effect.Aff (Aff, launchAff_)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useRef, readRefMaybe, useEffect)
import Unsafe.Coerce (unsafeCoerce)
import Markup as M
import React.Basic.Hooks as React
import React.Basic.Native as RN
import Animated (view, timing, value)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Native.Events (capture_)
import Record as Record
import Platform as Platform
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Icon (icon)
import Effect.Uncurried (EffectFn1)
import React.Basic.Native.Events (NativeSyntheticEvent)
import Paper (menu, menuItem)
import ApolloHooks (useApolloClient, Client)
import Data.Maybe (Maybe(..))

css :: forall css. { | css } -> CSS
css = unsafeCoerce
type Props = { shown :: Boolean, onLeftButtonPressed :: EffectFn1 (NativeSyntheticEvent RN.NativeTouchEvent) Unit, onRightButtonPressed :: Maybe Client -> EffectFn1 (NativeSyntheticEvent RN.NativeTouchEvent) Unit, title :: String}
headerStyles fade = {
  backgroundColor: "white",
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
  flex: 14,
  opacity: fade,
  zIndex: zIndex
}
  where
    zIndex :: Int
    zIndex = fade.interpolate {
      inputRange: [ 0, 1],
      outputRange: [-1, 9]
    }

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

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "TopBar") buildJsx

buildJsx props = React.do
  fade /\ setFade <- useState $ value 1
  client <- useApolloClient
  menuVisible /\ setMenuVisible <- useState false
  let
    menuButton = M.getJsx $ M.touchableOpacity { onPress: capture_ $ setMenuVisible $ \_ -> props.shown} do
       M.childElement icon {name: "gear", size: 34}

  useEffect props.shown do
     launchAff_ $ runAnimation props.shown fade
     pure mempty
  pure $ M.getJsx $ view {style: headerStyles fade} do
    M.touchableOpacity {style: M.css styles.backButton, onPress: props.onLeftButtonPressed} do
      M.childElement icon {name: "navicon", size: 34}
    M.text {style: M.css styles.title} do
      M.string props.title
    menu {visible: menuVisible, onDismiss: capture_ $ setMenuVisible $ \_ -> false, anchor: menuButton} do
      menuItem {onPress: props.onRightButtonPressed client, title: "Sign out"}
