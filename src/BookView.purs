module BookView where

import Effect (Effect)
import Prelude
import Effect.Console (log)
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
import AsyncStorage (clear, getItem)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)
import Paper (navigationOptions)
import ApolloHooks (useQuery, gql)
import Data.Either (either)
import Navigation (useFocusEffect)
import Reader as Reader

containerStyles =
  { flex: 1
  , backgroundColor: "white"
  }

barStyles showBars =
  { position: "absolute"
  , left: 0
  , right: 0
  , height: 55
  , zIndex: if showBars then 1 else -1
  }

type JSProps
  = { route ::
        { params ::
            { slug :: Nullable String
            }
        }
    , navigation ::
        { addListener ::
            EffectFn2 String
              ( EffectFn1
                  { preventDefault :: Effect Unit
                  }
                  Unit
              )
              (Effect Unit)
        }
    }

type Props
  = { route ::
        { params ::
            { slug :: String }
        }
    , navigation ::
        { addListener ::
            String ->
              ( { preventDefault :: Effect Unit } ->
              Effect Unit
            ) ->
            Effect (Effect Unit)
        }
    }

convertProps props =
  { route:
      { params: { slug: fromMaybe "" $ toMaybe props.route.params.slug } }
  , navigation: { addListener: \s f -> runEffectFn2 props.navigation.addListener s (mkEffectFn1 f)}
  }

reactComponent = navigationOptions c { headerShown: false }
  where
  c :: ReactComponent JSProps
  c =
    unsafePerformEffect
      $ do
          (component "BookView") buildJsx

callShow ref = do
  r <- readRefMaybe ref
  traverse_ _.show r

buildJsx jsProps = React.do
  let
    props = convertProps jsProps
  useEffect unit do
    unsubscribe <- props.navigation.addListener "tabPress" \e -> do
       log "HI"
       e.preventDefault
    pure unsubscribe
  let
    route = props.route
  location /\ setLocation <- useState "6"
  toc /\ setToc <- useState []
  height /\ setHeight <- useState 0.0
  width /\ setWidth <- useState 0.0
  title /\ setTitle <- useState ""
  sliderDisabled /\ setSliderDisabled <- useState true
  showBars /\ setShowBars <- useState true
  visibleLocation /\ setVisibleLocation <- useState { start: { percentage: 0, cfi: "0" } }
  showNav /\ setShowNav <- useState false
  useEffect title do
    launchAff_ do
      l <- getItem title
      liftEffect $ traverse_ (\l -> setLocation \_ -> l) l
    pure mempty
  let
    toggleBars = setShowBars $ \_ -> not $ showBars
  pure $ M.getJsx
    $ M.view
        { style: M.css containerStyles
        } do
        M.statusBar
          { hidden: not showBars
          , translucent: true
          , animated: false
          }
        M.childElement Reader.reactComponent
          { height
          , width
          , location
          , toggleBars
          , setToc
          , route
          , setTitle
          , title
          , setSliderDisabled
          , setVisibleLocation
          , visibleLocation
          , setHeight
          , setWidth
          , showBars
          , setShowBars
          }
        M.view
          { style: M.css $ Record.merge (barStyles showBars) { top: 0 }
          } do
          M.childElement TopBar.reactComponent
            { title: title
            , shown: showBars
            , onLeftButtonPressed: capture_ $ setShowNav \_ -> true
            , onRightButtonPressed:
                \client ->
                  capture_
                    $ launchAff_ do
                        clear
                        liftEffect $ traverse_ _.resetStore client
            } --, onLeftButtonPressed: liftEffect}
        M.view
          { style: M.css $ Record.merge (barStyles showBars) { bottom: 0 }
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
              , setShowNav: mkEffectFn1 \s -> setShowNav \_ -> s
              , display: mkEffectFn1 \loc -> setLocation \_ -> loc
              , toc: toc
              }
