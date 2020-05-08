module Home where
import Prelude
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Native as RN
import React.Basic.Native.Events as RNE
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\), useContext)
import Markup as M
import Paper (textInput, surface, button, listSection, listItem, listIcon, divider, title)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Record.Unsafe.Union (unsafeUnion)
import Data.Maybe (Maybe(..))
import ApolloHooks (useMutation, gql)
import Type.Proxy (Proxy(..))
import Navigation (useFocusEffect)
import AppTour (tourContext, useTourRef, showSequence)

type Props = { navigation :: { navigate :: EffectFn2 String {} Unit } }

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

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "Home") buildJsx

checkEmptyIcon total goal p
  | total >= goal = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-marked-outline" }
  | otherwise = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-blank-outline" }

buildJsx props = React.do
  {sequence, setSequence} <- useContext tourContext
  let redirectBook = runEffectFn2 props.navigation.navigate "Read" {}
  let redirectCreate = runEffectFn2 props.navigation.navigate "Create" {}
  let redirectReview = runEffectFn2 props.navigation.navigate "Review" {}
  let ratioDone numerator denominator p = M.getJsx $ M.text {style: M.css {marginTop: 8}} $ M.string $ numerator <> "/" <> denominator
  user <- useData (Proxy :: Proxy Query) query {fetchPolicy: "cache-and-network"}
  ref <- useTourRef {order: 1, title: "TESTING", description: "TESTING TESTIING"}
  useFocusEffect unit do
     user.refetch {}
     pure mempty

  useEffect unit do
     showSequence sequence
     pure unit
     pure mempty

  case user.state of
    Nothing -> mempty
    Just u ->
      pure $ M.getJsx
        $ M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface { style: M.css { flex: 1 } } do
              title { style: M.css {textAlign: "center"}} $ M.string "Daily Goals"
              listSection {} do
                listItem {key: "Pages Read", ref: ref, title: RN.string "Read 10 Pages", onPress: RNE.capture_ redirectBook, left: checkEmptyIcon u.currentUser.dailyReadPages 10, right: ratioDone (show u.currentUser.dailyReadPages) "10"}
                divider {style: M.css {height: 1, width: "100%"}}
                listItem {title: RN.string "Create 10 Flashcards", onPress: RNE.capture_ redirectCreate, left: checkEmptyIcon u.currentUser.dailyCreatedCards 10, right: ratioDone (show u.currentUser.dailyCreatedCards) "10"}
                divider {style: M.css {height: 1, width: "100%"}}
                listItem {title: RN.string "Review 30 Flashcards", onPress: RNE.capture_ redirectReview, left: checkEmptyIcon u.currentUser.dailyReviewedCards 30, right: ratioDone (show u.currentUser.dailyReviewedCards) "30"}
                divider {style: M.css {height: 1, width: "100%"}}

