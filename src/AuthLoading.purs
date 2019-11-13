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

type Props
  = {navigation :: {navigate :: EffectFn1 String Unit}}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "AuthLoading") buildJsx

type Book
  = { name :: String, slug :: String, __typename :: String, id :: String }

type User
  = {currentUser :: { firstName :: String, lastName :: String, email :: String, isGuest :: Boolean, books :: Array Book, id :: String, __typename :: String}}

query :: DocumentNode
query =
  gql
    """
query getUser {
  currentUser {
    id
    firstName
    lastName
    email
    isGuest
    books {
      id
      name
      slug
    }
  }
}
"""

buildJsx props = React.do
  queryResult <- useQuery query {}
  useEffect queryResult do
     effectForState queryResult props
     pure mempty
  pure $ domForState queryResult

domForState :: QueryState User -> JSX
domForState (Error e) =  RN.text {children: [RN.string $ spy "ERROR" e.message]}
--domForState (Data d) = RN.text {children: [RN.string $ spy "data" $ show d]}
domForState _ = RN.text {children: [RN.string "loading"]}

effectForState :: QueryState User -> Props -> Effect Unit
effectForState (Data d) props
  | spy "isGuest" d.currentUser.isGuest = runEffectFn1 (spy "nav" props.navigation.navigate) "Auth"
  | otherwise = runEffectFn1 (spy "nav" props.navigation.navigate) "App"
  
effectForState _ _ = spy "here" (pure unit)
