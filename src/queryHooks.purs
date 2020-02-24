module QueryHooks where

import Prelude
import Effect (Effect)
import React.Basic.Native as RN
import React.Basic.Hooks as React
import React.Basic.Hooks (useContext, useEffect, Hook, UseEffect, UseContext, JSX, coerceHook)
import Data.Newtype (class Newtype)
import Data.Either (Either(..))
import ApolloHooks (useQuery, gql, QueryState(..), DocumentNode)
import Context (dataStateContext, Context)
import Data.Maybe (Maybe(..))
import Type.Row (RProxy)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import Data.Eq (class EqRecord)
import Effect.Uncurried (runEffectFn1)
import Debug.Trace (spy)

type Book
  = { name :: String, slug :: String, __typename :: String, id :: String, filename :: String }

type User
  = { currentUser :: { firstName :: String, lastName :: String, email :: String, isGuest :: Boolean, books :: Array Book, id :: String, __typename :: String } }

userBooksQuery :: DocumentNode
userBooksQuery =
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
      filename
      slug
    }
  }
}
"""

useUserBooks opts = useData (Proxy :: Proxy User) userBooksQuery opts

newtype UseData (d :: Type) hooks
  = UseData (UseEffect (QueryState d) (UseContext Context (UseEffect Unit hooks)))

derive instance ntUseData :: Newtype (UseData d hooks) _

class Queryable (recordType :: # Type) where
  useData :: forall opts. Proxy (Record recordType) -> DocumentNode -> Record opts -> Hook (UseData (Record recordType)) (Maybe (Record recordType))

instance recordQueryable :: (RowToList a list, EqRecord list a) => Queryable (a) where
  useData _ query options =
    coerceHook
      $ React.do
          result <- useQuery query options
          { setLoading, setError } <- useContext dataStateContext
          useEffect result
            $ do
                dataEffect (runEffectFn1 setLoading) (runEffectFn1 setError) result
                pure $ mempty
          pure $ fetchData result

dataEffect :: forall d. ((Boolean -> Boolean) -> Effect Unit) -> (String -> Effect Unit) -> QueryState (Record d) -> Effect Unit
dataEffect setLoading setError (Data _) = pure unit

dataEffect setLoading setError Loading = setLoading \_ -> true

dataEffect setLoading setError (Error e) = setError $ spy "message" e.message

fetchData :: forall a. QueryState a -> Maybe a
fetchData (Data d) = Just d

fetchData Loading = Nothing

fetchData (Error e) = Nothing
