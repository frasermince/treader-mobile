module App where

import Effect (Effect)
import Prelude
import Effect.Class (liftEffect)
import EpubRn (epub, startStream, streamGet, killStream)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import Effect.Uncurried (mkEffectFn1)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..))
import React.Basic.Native as RN
import Markup as M
import React.Basic.Hooks as React
import React.Basic.Native.Events (capture_)
import React.Basic.Native.Events as RNE
import React.Basic.Events (merge, EventFn)
import Effect.Console (log)
import Record as Record
import TopBar as TopBar

styles = {
  container: {
    flex: 1
  },
  wrapper: {
    flex: 1,
    marginTop: 40,
    marginBottom: 40
  },
  reader: {
    flex: 1,
    alignSelf: "stretch",
    backgroundColor: "#3F3F3C"
  },
  bar: {
    position:"absolute",
    left:0,
    right:0,
    height:55
  }
}

type Props = {}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "App") buildJsx

layoutEvent setHeight setWidth = mkEffectFn1 e
  where e :: RN.LayoutChangeEvent -> Effect Unit
        e event = do
          let {x, y, width, height} = event.nativeEvent.layout
          setHeight \_ -> height
          setWidth \_ -> height

locationChange setLocation = mkEffectFn1 e
  where e :: String -> Effect Unit
        e location = setLocation \_ -> location

locationsReady setSliderDisabled = mkEffectFn1 e
   where e :: String -> Effect Unit
         e locations = setSliderDisabled \_ -> false

ready setTitle setToc = mkEffectFn1 e
  where e :: {package :: {metadata :: {title :: String}}, navigation :: {toc :: Array String}} -> Effect Unit
        e book = do
           setTitle \_ -> book.package.metadata.title
           setToc \_ -> book.navigation.toc

press toggleBars = mkEffectFn1 e
  where e :: {} -> Effect Unit
        e book = toggleBars

error = mkEffectFn1 e
  where e :: String -> Effect Unit
        e message = log $ "EPUBJS-Webview " <> message

buildJsx props = React.do
  flow /\ setFlow <- useState "paginated"
  location /\ setLocation <- useState "6"
  url /\ setUrl <- useState  "https://s3.amazonaws.com/epubjs/books/moby-dick.epub"
  src /\ setSrc <- useState ""
  origin /\ setOrigin <- useState ""
  title /\ setTitle <- useState ""
  toc /\ setToc <- useState []
  height /\ setHeight <- useState 0.0
  width /\ setWidth <- useState 0.0
  showBars /\ setShowBars <- useState true
  showNav /\ setShowNav <- useState false
  sliderDisabled /\ setSliderDisabled <- useState true
  let toggleBars = setShowBars $ \_ -> not showBars

  useEffect unit do
    launchAff_ $ do
       let delayAndToggle = do
             delay $ Milliseconds 1000.0
             liftEffect $ toggleBars
       fiber <- forkAff $ delayAndToggle
       origin <- startStream
       liftEffect $ setOrigin $ \_ -> origin
       src <- streamGet origin
       liftEffect $ setSrc $ \_ -> src
    pure $ killStream

  pure $ M.getJsx $ M.view {style: M.css styles.container} do
    M.statusBar {hidden: not showBars, translucent: true, animated: false}
    M.view {style: M.css styles.wrapper, onLayout: layoutEvent setHeight setWidth} do
       M.childElement epub {style: M.css styles.reader, height: height, width: width, src: src, flow: flow, locaation: location, onLocationChange: locationChange setLocation, onLocationsReady: locationsReady setSliderDisabled, onReady: ready setTitle setToc, onPress: press toggleBars, origin: origin, onError: error }
    M.view {style: M.css $ Record.merge styles.bar {top: 0}} do
       M.childElement TopBar.reactComponent {title: title, shown: showBars, onLeftButtonPressed: capture_ $ pure unit, onRightButtonPressed: capture_ $ pure unit}--, onLeftButtonPressed: liftEffect}

