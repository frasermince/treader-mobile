module AuthLoading where

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

type Props
  = { navigation :: { navigate :: EffectFn1 String Unit } }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "AuthLoading") buildJsx

buildJsx props = React.do
  queryResult <- useUserBooks {}
  useEffect queryResult do
    traverse_ (effectForState props) queryResult
    pure mempty
  pure $ mempty

effectForState :: Props -> User -> Effect Unit
effectForState props d
  | d.currentUser.isGuest = runEffectFn1 props.navigation.navigate "Auth"
  | otherwise = runEffectFn1 props.navigation.navigate "App"
