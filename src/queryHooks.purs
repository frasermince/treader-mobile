module QueryHooks where

import Prelude
import Effect (Effect)
import React.Basic.Native as RN
import React.Basic.Hooks as React
import React.Basic.Hooks (useContext, useEffect, Hook, UseEffect, UseContext, JSX, coerceHook)
import Data.Newtype (class Newtype)
import Data.Either (Either(..))
import ApolloHooks (useQuery, gql, QueryState(..), DocumentNode, QueryResult)
import Context (dataStateContext, Context)
import Data.Maybe (Maybe(..))
import Type.Row (RProxy)
import Prim.RowList (class RowToList)
import Type.Proxy (Proxy(..))
import Data.Eq (class EqRecord)
import Effect.Uncurried (runEffectFn1)
import Data.Nullable (toMaybe, Nullable)
import Debug.Trace (spy)
import Data.String (stripPrefix, Pattern(..))
import Data.Maybe (fromMaybe)
import Prim.Row (class Union)

type Book
  = { name :: String, slug :: String, __typename :: String, id :: String, filename :: String, language :: String, audioChapters :: Array {chapters :: String, audioUrl :: String} }

type User
  = { currentUser :: { firstName :: String, lastName :: String, email :: String, isGuest :: Boolean, iosVersion :: Nullable String, isSubscribed :: Boolean, isPermitted :: Boolean, books :: Array Book, id :: String,  __typename :: String, subscriptionEndDate :: Nullable Number, showPayment :: Nullable Boolean, language :: String } }

userBooksQuery :: DocumentNode
userBooksQuery =
  gql
    """
query getUser {
  currentUser {
    id
    firstName
    lastName
    language
    email
    isPermitted
    isSubscribed
    isGuest
    iosVersion
    showPayment
    subscriptionEndDate
    books {
      id
      name
      filename
      slug
      language
      audioChapters {
        chapter
        audioUrl
      }
    }
  }
}
"""

useUserBooks opts = useData (Proxy :: Proxy User) userBooksQuery opts

newtype UseData (d :: Type) hooks
  = UseData (UseEffect (QueryState d) (UseContext Context (UseEffect Unit hooks)))

type DataResult r s = {state :: Maybe r, refetch :: Record s -> Effect Unit, networkStatus :: Int}
derive instance ntUseData :: Newtype (UseData d hooks) _

class Queryable (recordType :: # Type) where
  useData :: forall opts refetch . Proxy (Record recordType) -> DocumentNode -> Record opts -> Hook (UseData (Record recordType)) (DataResult (Record recordType) refetch)

instance recordQueryable :: (RowToList a list, EqRecord list a) => Queryable (a) where
  useData _ query options =
    coerceHook
      $ React.do
          result <- useQuery query options
          { setLoading, setError } <- useContext dataStateContext
          useEffect result.state
            $ do
                dataEffect (runEffectFn1 setLoading) (runEffectFn1 setError) result.state
                pure $ mempty
          pure {state: fetchData result.state, refetch: result.refetch, networkStatus: result.networkStatus }

dataEffect :: forall d. ((Boolean -> Boolean) -> Effect Unit) -> (String -> Effect Unit) -> QueryState (Record d) -> Effect Unit
dataEffect setLoading setError (Data _) = do
  setLoading \_ -> false
  pure unit

dataEffect setLoading setError Loading = setLoading \_ -> true

dataEffect setLoading setError (Error e) = do
  setLoading \_ -> false
  setError $ spy "message" e.message

fetchData :: forall a. QueryState a -> Maybe a
fetchData (Data d) = Just d

fetchData Loading = Nothing

fetchData (Error e) = Nothing

stripGraphqlError message = fromMaybe message $ stripPrefix (Pattern "GraphQL error: ") message
