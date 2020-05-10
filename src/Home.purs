module Home where
import Prelude
import Effect (Effect)
import Effect.Class (liftEffect)
import Control.Promise as Promise
import Effect.Aff (Aff, launchAff_)
import Control.Promise (Promise)
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Native as RN
import React.Basic.Native.Events as RNE
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\))
import Markup as M
import Paper (textInput, surface, button, listSection, listItem, listIcon, divider, title, _listItem)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Record.Unsafe.Union (unsafeUnion)
import Data.Maybe (Maybe(..))
import ApolloHooks (useMutation, gql)
import Type.Proxy (Proxy(..))
import Navigation (useFocusEffect)
import Copilot (_copilot, copilotStep, walkthroughableChild)
import Debug.Trace (spy)
import Data.Nullable (null, Nullable)
import Web.DOM.Internal.Types (Node)

type JSProps = { navigation :: { navigate :: EffectFn2 String {} Unit }, start :: EffectFn2 (Nullable String) (Nullable Node) (Promise Unit) }

type Props = { navigation :: { navigate :: String -> {} -> Effect Unit }, start :: Nullable String -> Nullable Node -> Aff Unit}
type Query
  = { currentUser :: {dailyReviewedCards :: Int, dailyCreatedCards :: Int, dailyReadPages :: Int, id :: String} }
query =
  gql
    """
query getUser {
  currentUser {
    id
    dailyReviewedCards
    dailyCreatedCards
    dailyReadPages
  }
}
"""

reactComponent :: ReactComponent JSProps
reactComponent =
  _copilot {overlay: "svg", animated: true} $ unsafePerformEffect
    $ do
        (component "Home") buildJsx

checkEmptyIcon total goal p
  | total >= goal = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-marked-outline" }
  | otherwise = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-blank-outline" }

convertProps :: JSProps -> Props
convertProps jsProps = {navigation: {navigate: runEffectFn2 jsProps.navigation.navigate}, start: startAff}
  where startAff :: Nullable String -> Nullable Node -> Aff Unit
        startAff a b = liftEffect (runEffectFn2 jsProps.start a b) >>= Promise.toAff

buildJsx jsProps = React.do
  let props = convertProps jsProps
  let redirectBook = props.navigation.navigate "Read" {}
  let redirectCreate = props.navigation.navigate "Create" {}
  let redirectReview = props.navigation.navigate "Review" {}
  let ratioDone numerator denominator p = M.getJsx $ M.text {style: M.css {marginTop: 8}} $ M.string $ numerator <> "/" <> denominator
  let walkthroughableListItem = walkthroughableChild _listItem
  user <- useData (Proxy :: Proxy Query) query {fetchPolicy: "cache-and-network"}
  useEffect unit do
    launchAff_ $ spy "START" $ props.start null null
    pure mempty

  useFocusEffect unit do
     user.refetch {}
     pure mempty

  case user.state of
    Nothing -> mempty
    Just u ->
      pure $ M.getJsx
        $ M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface { style: M.css { flex: 1 } } do
              title { style: M.css {textAlign: "center"}} $ M.string "Daily Goals"
              listSection {} do
                copilotStep {text: RN.string "Unchart consists of three steps. As show here. This page will track your daily progress and you click on any of these to go the appropriate page.", order: 1, name: RN.string "TEST"} do
                    walkthroughableListItem {title: RN.string "Read 10 Pages", onPress: RNE.capture_ redirectBook, left: checkEmptyIcon u.currentUser.dailyReadPages 10, right: ratioDone (show u.currentUser.dailyReadPages) "10"}
                divider {style: M.css {height: 1, width: "100%"}}

                copilotStep {text: RN.string "This will show you ", order: 2, name: RN.string "TEST"} do
                   walkthroughableListItem {title: RN.string "Create 10 Flashcards", onPress: RNE.capture_ redirectCreate, left: checkEmptyIcon u.currentUser.dailyCreatedCards 10, right: ratioDone (show u.currentUser.dailyCreatedCards) "10"}
                divider {style: M.css {height: 1, width: "100%"}}
                listItem {title: RN.string "Review 30 Flashcards", onPress: RNE.capture_ redirectReview, left: checkEmptyIcon u.currentUser.dailyReviewedCards 30, right: ratioDone (show u.currentUser.dailyReviewedCards) "30"}
                divider {style: M.css {height: 1, width: "100%"}}

