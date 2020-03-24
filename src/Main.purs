module Main where

import Prelude
import Debug.Trace
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useEffect)
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
import SignIn as SignIn
import Paper (portal)
import Debug.Trace (spy)

type Props = {}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "Main") buildJsx

buildJsx props = React.do
  queryResult <- useUserBooks {}
  pure $ M.getJsx $ traverse_ authOrApp queryResult

authOrApp :: User -> M.Markup Unit
authOrApp d
  | d.currentUser.isGuest = authenticationNavigator {}
  | (isNothing $ toMaybe d.currentUser.iosVersion) && not d.currentUser.isSubscribed = spy "PORTAL" $ portal {} $ M.childElement Subscribe.reactComponent {visible: true, onDismiss: mempty}
  | otherwise = tabNavigator {}
