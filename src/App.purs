module App where

import Effect (Effect)
import Prelude
import Debug.Trace (spy)
import Effect.Class (liftEffect)
import EpubRn (epub, createStreamer, startStream, streamGet, killStream)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef)
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
import BottomBar as BottomBar
import Nav (nav)
import Data.Nullable (null)
import Data.Traversable (traverse_)
import Data.Int (fromString, floor)
import Data.Maybe (Maybe(..))
import AsyncStorage (clear)
import Effect.Uncurried (runEffectFn1, EffectFn1)
import Paper (navigationOptions)

styles =
  { container:
    { flex: 1
    }
  , wrapper:
    { flex: 1
    , marginTop: 40
    , marginBottom: 40
    }
  , reader:
    { flex: 1
    , alignSelf: "stretch"
    , backgroundColor: "#3F3F3C"
    }
  , bar:
    { position: "absolute"
    , left: 0
    , right: 0
    , height: 55
    }
  }

type Props
  = {navigation :: {navigate :: EffectFn1 String Unit}}
reactComponent = navigationOptions c {headerShown: false}
  where c :: ReactComponent Props
        c = unsafePerformEffect $ do
           (component "App") buildJsx

layoutEvent setHeight setWidth = mkEffectFn1 e
  where
  e :: RN.LayoutChangeEvent -> Effect Unit
  e event = do
    let
      { x, y, width, height } = (spy "event" event).nativeEvent.layout
    _ <- setHeight \_ -> height
    _ <- setWidth \_ -> width
    pure unit

locationChange setVisibleLocation = mkEffectFn1 e
  where
  e :: {start :: {percentage :: Int}} -> Effect Unit
  e event = setVisibleLocation \_ -> event

locationsReady setSliderDisabled = mkEffectFn1 e
  where
  e :: String -> Effect Unit
  e locations = setSliderDisabled \_ -> false

ready setTitle setToc = mkEffectFn1 e
  where
  e ::
    { package ::
      { metadata :: { title :: String } }
    , navigation :: { toc :: Array String }
    } ->
    Effect Unit
  e book = do
    setTitle \_ -> book.package.metadata.title
    setToc \_ -> book.navigation.toc

press toggleBars = mkEffectFn1 e
  where
  e :: {} -> Effect Unit
  e book = toggleBars

error = mkEffectFn1 e
  where
  e :: String -> Effect Unit
  e message = log $ "EPUBJS-Webview " <> message

callShow ref = do
  r <- readRefMaybe ref
  traverse_ _.show r

buildJsx props = React.do
  flow /\ setFlow <- useState "paginated"
  location /\ setLocation <- useState "6"
  url /\ setUrl <- useState "https://s3.amazonaws.com/epubjs/books/moby-dick.epub"
  src /\ setSrc <- useState ""
  origin /\ setOrigin <- useState ""
  title /\ setTitle <- useState ""
  toc /\ setToc <- useState []
  height /\ setHeight <- useState 0.0
  width /\ setWidth <- useState 0.0
  showBars /\ setShowBars <- useState true
  showNav /\ setShowNav <- useState false
  sliderDisabled /\ setSliderDisabled <- useState true
  visibleLocation /\ setVisibleLocation <- useState {start: {percentage: 0}}
  showNav /\ setShowNav <- useState false
  let
    streamer = createStreamer
  let
    toggleBars = setShowBars $ \_ -> not showBars
  useEffect unit do
    launchAff_
      $ do
          let
            delayAndToggle = do
              delay $ Milliseconds 1000.0
              liftEffect $ toggleBars
          fiber <- forkAff $ delayAndToggle
          origin <- (startStream streamer)
          liftEffect $ setOrigin $ \_ -> origin
          src <- streamGet streamer url
          liftEffect $ setSrc $ \_ -> src
    pure $ killStream streamer
  pure $ M.getJsx
    $ M.view
        { style: M.css styles.container
        } do
        M.statusBar
          { hidden: not showBars
          , translucent: true
          , animated: false
          }
        M.view
          { style: M.css styles.wrapper
          , onLayout: layoutEvent setHeight setWidth
          } do
          M.childElement epub
            { style: M.css styles.reader
            , height: height
            , width: width
            , src: src
            , flow: flow
            , location: location
            , onLocationChange: locationChange setVisibleLocation
            , onLocationsReady: locationsReady setSliderDisabled
            , onReady: ready setTitle setToc
            , onPress: press toggleBars
            , origin: origin
            , onError: error
            }
        M.view
          { style: M.css $ Record.merge styles.bar { top: 0 }
          } do
          M.childElement TopBar.reactComponent
            { title: title
            , shown: showBars
            , onLeftButtonPressed: capture_ $ setShowNav \_ -> true
            , onRightButtonPressed: capture_ $ launchAff_ do
               clear
               liftEffect $ runEffectFn1 props.navigation.navigate "Auth"
            } --, onLeftButtonPressed: liftEffect}
        M.view
          { style: M.css $ Record.merge styles.bar { bottom: 0 }
          } do
          M.childElement BottomBar.reactComponent
            { disabled: sliderDisabled
            , value: visibleLocation.start.percentage
            , shown: showBars
            , onSlidingComplete: \number -> setLocation \_ -> show number
            }
          M.view {} do
            M.childElement nav
              { shown: showNav
              , display: mkEffectFn1 \loc -> setLocation \_ -> loc
              , toc: toc
              }
