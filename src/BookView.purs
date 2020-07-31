module BookView where

import Effect (Effect)
import Prelude
import Type.Proxy (Proxy(..))
import ComponentTypes (BookViewQuery, AudioInformation)
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
import AsyncStorage (clear, getItem, removeItem)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)
import Paper (navigationOptions)
import ApolloHooks (useQuery, gql)
import Data.Either (either)
import Reader as Reader
import QueryHooks (useData, UseData, stripGraphqlError)

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
  = { route :: Nullable { params :: Nullable { slug :: Nullable String } }, navigation :: { navigate :: EffectFn2 String {} Unit }}

type Props
  = {  route :: { params :: { slug :: Maybe String } }, navigation :: { navigate :: EffectFn2 String {} Unit }}

query =
  gql
    """
  query routes_Book_Query($book: String) {
    book(slug: $book) {
      id
      epubUrl
      processedEpubUrl
      audioChapters {
        chapter
        audioUrl
      }
    }
  }
"""

convertProps props =
  { route: { params: { slug: slug} },
    navigation: props.navigation
  }
  where slug = do
          r <- toMaybe props.route
          p <- toMaybe r.params
          toMaybe p.slug

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
  let
    route = props.route
  bookData <- useData (Proxy :: Proxy BookViewQuery) query { variables: { book: fromMaybe "" route.params.slug }, errorPolicy: "all" }
  location /\ setLocation <- useState "0"
  toc /\ setToc <- useState []
  height /\ setHeight <- useState 0.0
  width /\ setWidth <- useState 0.0
  title /\ setTitle <- useState Nothing
  sliderDisabled /\ setSliderDisabled <- useState true
  showBars /\ setShowBars <- useState true
  visibleLocation /\ setVisibleLocation <- useState { start: { percentage: 0, cfi: "0" } }
  audioInformation /\ setAudioInformation <- useState (Nothing :: Maybe AudioInformation)
  showNav /\ setShowNav <- useState false
  let
    toggleBars = setShowBars $ \_ -> not $ showBars
  pure $ M.getJsx $ fromMaybe mempty $ route.params.slug <#> \slug ->
    M.view
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
          , slug
          , setTitle
          , title
          , setSliderDisabled
          , setVisibleLocation
          , visibleLocation
          , setAudioInformation
          , setHeight
          , setWidth
          , bookData: bookData.state
          , showBars
          , setShowBars
          , setLocation
          }
        M.view
          { style: M.css $ Record.merge (barStyles showBars) { top: 0 }
          } do
          M.childElement TopBar.reactComponent
            { title: title
            , shown: showBars
            , onLeftButtonPressed: capture_ $ runEffectFn2 props.navigation.navigate "BookIndex" {}
            , onRightButtonPressed: capture_ $ setShowNav \_ -> true
            }

        M.childElement BottomBar.reactComponent
            { disabled: sliderDisabled
            , value: visibleLocation.start.percentage
            , bookData: bookData.state
            , shown: showBars
            , onSlidingComplete: \number -> setLocation \_ -> show number
            , slug: slug
            , audioInformation
            , visibleLocation
            }
