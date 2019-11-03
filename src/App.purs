module App where

import Prelude
import Effect.Class (liftEffect)
import EpubRn (epub, startStream, streamGet, killStream)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..))
import React.Basic.Native as RN
import Markup as M
import React.Basic.Hooks as React

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


buildJsx props = React.do
  flow /\ setFlow <- useState "paginated"
  location /\ setLocation <- useState 6
  url /\ setUrl <- useState  "https://s3.amazonaws.com/epubjs/books/moby-dick.epub"
  src /\ setSrc <- useState ""
  origin /\ setOrigin <- useState ""
  title /\ setTitle <- useState ""
  toc /\ setToc <- useState []
  height /\ setHeight <- useState 0
  width /\ setWidth <- useState 0
  showBars /\ setShowBars <- useState true
  showNav /\ setShowNav <- useState false
  sliderDisabled /\ setSliderDisabled <- useState true

  useEffect unit do
    launchAff_ $ do
       let toggleBars = do
             delay $ Milliseconds 1000.0
             liftEffect $ setShowBars $ \_ -> not showBars
       fiber <- forkAff $ toggleBars
       origin <- startStream
       liftEffect $ setOrigin $ \_ -> origin
       src <- streamGet origin
       liftEffect $ setSrc $ \_ -> src
    pure $ killStream
  pure $ M.getJsx $ M.view {style: M.css styles.container} do
    M.statusBar {hidden: not showBars, translucent: true, animated: false}
    M.view {style: M.css styles.wrapper--, onLayout: \event -> do
              --let {x, y, width, height} = event.nativeEvent.layout
              --setHeight \_ -> height
              --setWidth \_ -> height
           } (M.string "hi")



