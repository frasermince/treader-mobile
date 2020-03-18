module Subscribe where

import Prelude
import React.Basic.Hooks as React
import Effect.Unsafe (unsafePerformEffect)
import Paper (surface, title, divider, button, modal, subheading, headline)
import React.Basic.Hooks (JSX, ReactComponent, component)
import Markup as M
import Swiper (swiper)
import Debug.Trace (spy)
import React.Basic.Native.Events as RNE
import Effect (Effect)
import Icon (icon)

type Props = {visible :: Boolean, setVisible :: (Boolean -> Boolean) -> Effect Unit}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Reader" $ buildJsx

buildJsx props = React.do
  pure $ M.getJsx do
      let dismiss = props.setVisible \_ -> false
      modal {visible: props.visible, contentContainerStyle: modalStyle, onDismiss: dismiss} do
          M.view {style: surfaceStyle} do
            M.view {style: benefitsSectionStyle} do
              title {} $ M.string "Upgrade to Unchart Premium"
              swiper {style: M.css {}, horizontal: true, autoplay: true, showButtons: true, loop: true} do
                  M.view {style: slideStyle} do
                     subheading {style: textStyle} $ M.string "Translate Unlimited Words"
                  M.view {style: slideStyle} $ subheading {style: textStyle} $ M.string "Upload Your Own Books"

            M.view {style: priceSectionStyle} do
              subheading {} $ M.string "Upgrade to Premium for"
              M.text {style: priceStyle} $ M.string "$11.99/mo"
              M.view {style: bottomStyle} do
                button { mode: "contained", style: mainButtonStyle } $ M.string "CONTINUE"
                button {onPress: RNE.capture_ $ dismiss} $ M.string "NO THANKS"
          M.view {style: bottomViewStyle} do
            M.text {style: M.css {color: "white"}} $ M.string "Recurring billing, cancel anytime"

benefitsSectionStyle = M.css
  {
    justifyContent: "center",
    alignItems: "center",
    width: "100%",
    flex: 1,
    borderBottomColor: "b2b2b2",
    borderBottomWidth: 0.5,
    borderRadius: 10,
    paddingTop: 30
  }

priceSectionStyle = M.css
  {
    alignItems: "center",
    width: "100%",
    paddingTop: 15,
    flex: 1
  }

bottomViewStyle = M.css
  {
    justifyContent: "center",
    alignItems: "center",
    flex: 1
  }

modalStyle = M.css
  {
    paddingTop: 0,
    margingTop: 0,
    paddingLeft: "4%",
    width: "95%",
    height: "100%"
  }

surfaceStyle = M.css
  {
    borderRadius: 10,
    backgroundColor: "white",
    justifyContent: "center",
    alignItems: "center",
    width: "100%",
    flex: 4
  }

slideStyle = M.css
  {
    justifyContent: "center",
    alignItems: "center",
    height: "100%"
  }

mainButtonStyle = M.css
  {
    marginBottom: 20,
    width: 300,
    height: 40,
    textSize: 50
  }

bottomStyle = M.css
  {
    flex: 1,
    justifyContent: "flex-end",
    marginBottom: 16
  }

priceStyle = M.css
  {
    marginTop: 10,
    fontWeight: "700",
    fontSize: 28
  }

textStyle = M.css
  {
    color: "black"
  }
