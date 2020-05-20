module Home where
import Prelude
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Native as RN
import React.Basic.Native.Events as RNE
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\))
import Markup as M
import Paper (textInput, surface, button, listSection, listItem, listIcon, divider, title)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Record.Unsafe.Union (unsafeUnion)
import Data.Maybe (Maybe(..))
import ApolloHooks (useMutation, gql)
import Type.Proxy (Proxy(..))
import Navigation (useFocusEffect)
import FirebaseMessaging (requestPermission)
import Effect.Aff (Aff, launchAff_, try)

type Props = { navigation :: { navigate :: EffectFn2 String {} Unit } }

chevron p = element listIcon $ unsafeUnion p { color: "#000", icon: "chevron-right" }
type Query
  = { currentUser :: {dailyReviewedSessions :: Int, dailyCreatedCards :: Int, dailyReadPages :: Int, id :: String, dailyGoal :: {pages :: Int, reviewSessions :: Int, created :: Int}} }
query =
  gql
    """
query getUser {
  currentUser {
    id
    dailyReviewedSessions
    dailyCreatedCards
    dailyReadPages
    dailyGoal {
      created
      pages
      reviewSessions
    }
  }
}
"""

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "Home") buildJsx

checkEmptyIcon total goal p
  | total >= goal = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-marked-outline" }
  | otherwise = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-blank-outline" }

buildJsx props = React.do
  let redirectBook = runEffectFn2 props.navigation.navigate "Read" {}
  let redirectCreate = runEffectFn2 props.navigation.navigate "Create" {}
  let redirectReview = runEffectFn2 props.navigation.navigate "Review" {}
  let ratioDone numerator denominator = numerator <> "/" <> denominator
  user <- useData (Proxy :: Proxy Query) query {fetchPolicy: "cache-and-network"}
  useEffect unit do
    launchAff_ requestPermission
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
                listItem {title: RN.string $ (ratioDone (show u.currentUser.dailyReadPages) (show u.currentUser.dailyGoal.pages)) <> " Pages Read", onPress: RNE.capture_ redirectBook, left: checkEmptyIcon u.currentUser.dailyReadPages u.currentUser.dailyGoal.pages, right: chevron}
                divider {style: M.css {height: 1, width: "100%"}}
                listItem {title: RN.string $ (ratioDone (show u.currentUser.dailyCreatedCards) (show u.currentUser.dailyGoal.created)) <> " Flashcards Created", onPress: RNE.capture_ redirectCreate, left: checkEmptyIcon u.currentUser.dailyCreatedCards u.currentUser.dailyGoal.created, right: chevron}
                divider {style: M.css {height: 1, width: "100%"}}
                listItem {title: RN.string $ (ratioDone (show u.currentUser.dailyReviewedSessions) (show u.currentUser.dailyGoal.reviewSessions)) <> " Sessions Completed", onPress: RNE.capture_ redirectReview, left: checkEmptyIcon u.currentUser.dailyReviewedSessions u.currentUser.dailyGoal.reviewSessions, right: chevron}
                divider {style: M.css {height: 1, width: "100%"}}

