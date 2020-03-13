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
import Data.Maybe (fromMaybe)
import Data.Eq (class Eq)
import Data.Traversable (traverse_)
import QueryHooks (useUserBooks, Book, User)
import Markup as M
import TabNavigator (tabNavigator)
import AuthenticationNavigator (authenticationNavigator)
import Subscribe (subscribe)
import SignIn as SignIn

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
  | not d.currentUser.isSubscribed = subscribe {}
  | otherwise = tabNavigator {}
