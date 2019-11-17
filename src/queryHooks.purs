module QueryHooks where
import Prelude
import React.Basic.Native as RN
import ApolloHooks (useQuery, gql, QueryState(..), DocumentNode)

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

useUserBooks opts = useQuery query opts

handleState (Loading) fn = RN.text {children: [RN.string "loading"]}
handleState (Error e) fn = RN.text {children: [RN.string $ e.message]}
handleState (Data d) fn = fn d
