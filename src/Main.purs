module Main where

import Prelude
import Debug.Trace
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useEffect, (/\))
import ApolloHooks (useQuery, gql, QueryState(..), DocumentNode)
import React.Basic.Native as RN
import Effect.Uncurried (runEffectFn1, EffectFn1)
import React.Basic.Hooks as React
import Data.Array (head)
import Data.Maybe (fromMaybe, isNothing)
import Data.Eq (class Eq)
import Data.Traversable (traverse_)
import QueryHooks (useUserBooks, Book, User)
import Markup as M
import TabNavigator (tabNavigator)
import AuthenticationNavigator (authenticationNavigator)
import Data.Nullable (toMaybe, Nullable)
import Subscribe as Subscribe
import Introduction as Introduction
import SignIn as SignIn
import Paper (portal)
import Effect.Aff (Aff, launchAff_, try)
import Debug.Trace (spy)
import ApolloHooks (useMutation, gql, DocumentNode)

type Props = {}

mutation :: DocumentNode
mutation =
  gql
    """
mutation updateCurrentUser($input: UserInput!) {
  update_user(input: $input) {
    user {
      id
      isSubscribed
      isPermitted
      showPayment
    }
  }
}
  """

dismiss mutationFn = launchAff_ $ mutationFn {variables: {input: {showPayment: false}}}

isCurrentVersion d = fromMaybe false $ do
  v <- toMaybe d.currentUser.iosVersion
  pure $ v >= "1.4.6"

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "Main") buildJsx

buildJsx props = React.do
  mutationFn /\ result <- useMutation mutation {}
  queryResult <- useUserBooks {fetchPolicy: "cache-and-network"}
  pure $ M.getJsx $ traverse_ (authOrApp (dismiss mutationFn)) (spy "RESULT" queryResult).state

authOrApp :: Effect Unit -> User -> M.Markup Unit
authOrApp onDismiss d
  | d.currentUser.isGuest = authenticationNavigator {}
  | (fromMaybe true $ toMaybe d.currentUser.showPayment) && not d.currentUser.isSubscribed =
      portal {} $ M.childElement Subscribe.reactComponent {visible: true, onDismiss: onDismiss}
  | not $ isCurrentVersion d = M.childElement Introduction.reactComponent {}
  | otherwise = tabNavigator {}
