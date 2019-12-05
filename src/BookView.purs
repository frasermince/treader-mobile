module BookView where

import Effect (Effect)
import Prelude
import Debug.Trace (spy)
import Effect.Class (liftEffect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..))
import React.Basic.Native as RN
import Markup as M
import React.Basic.Hooks as React
import React.Basic.Native.Events (capture_)
import React.Basic.Native.Events as RNE
import React.Basic.Events (merge, EventFn)
import Record as Record
import TopBar as TopBar
import BottomBar as BottomBar
import Nav (nav)
import Data.Nullable (null)
import Data.Traversable (traverse_)
import Data.Int (fromString, floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Nullable (Nullable, toMaybe)
import AsyncStorage (clear)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)
import Paper (navigationOptions)
import ApolloHooks (useQuery, gql)
import Data.Either (either)
import Reader as Reader

styles =
  { container:
    { flex: 1
    }
  , wrapper:
    { flex: 1
    , marginTop: 40
    , marginBottom: 40
    }
  , bar:
    { position: "absolute"
    , left: 0
    , right: 0
    , height: 55
    }
  }

type JSProps
  = {navigation :: {navigate :: EffectFn1 String Unit, state :: {params :: {slug :: Nullable String}}}}

type Props = {navigation :: {navigate :: String -> Effect Unit, state :: {params :: {slug :: String}}}}

convertProps props = {
  navigation: {
    navigate: runEffectFn1 props.navigation.navigate,
    state: {params: {slug: fromMaybe "" $ toMaybe props.navigation.state.params.slug}}
  }
}
reactComponent = navigationOptions c {headerShown: false}
  where c :: ReactComponent JSProps
        c = unsafePerformEffect $ do
           (component "BookView") buildJsx

layoutEvent setHeight setWidth = mkEffectFn1 e
  where
  e :: RN.LayoutChangeEvent -> Effect Unit
  e event = do
    let
      { x, y, width, height } = (spy "event" event).nativeEvent.layout
    _ <- setHeight \_ -> height
    _ <- setWidth \_ -> width
    pure unit

callShow ref = do
  r <- readRefMaybe ref
  traverse_ _.show r

buildJsx jsProps = React.do
  let props = convertProps jsProps
  let navigation = props.navigation
  location /\ setLocation <- useState "6"
  toc /\ setToc <- useState []
  height /\ setHeight <- useState 0.0
  width /\ setWidth <- useState 0.0
  title /\ setTitle <- useState ""
  sliderDisabled /\ setSliderDisabled <- useState true
  showBars /\ setShowBars <- useState true
  visibleLocation /\ setVisibleLocation <- useState {start: {percentage: 0}}
  showNav /\ setShowNav <- useState false

  let
    toggleBars = setShowBars $ \_ -> not showBars
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
          M.childElement Reader.reactComponent {
            height,
            width,
            location,
            toggleBars,
            setToc,
            navigation,
            setTitle,
            setSliderDisabled,
            setVisibleLocation
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
              liftEffect $ props.navigation.navigate "Auth"
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
