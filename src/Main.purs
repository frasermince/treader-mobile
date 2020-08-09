module Main where

import Prelude
import Debug.Trace
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useEffect, (/\), useContext)
import ApolloHooks (useQuery, gql, QueryState(..), DocumentNode)
import React.Basic.Native as RN
import Effect.Console (log)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Platform as Platform
import Effect.Uncurried (runEffectFn1, EffectFn1)
import React.Basic.Hooks as React
import Data.Array (head)
import Data.Maybe (fromMaybe, isNothing)
import Data.Eq (class Eq)
import Data.Traversable (traverse_)
import QueryHooks (useUserBooks, Book, User, stripGraphqlError)
import Effect.Exception (message)
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
import Effect.Class (liftEffect)
import InAppPurchases (requestSubscription, getSubscriptions, purchaseUpdatedListener, purchaseErrorListener, finishTransaction)
import Data.Either (Either(..))
import Context (dataStateContext, Context)
import Segment (track, screen)

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

subscribeMutation :: DocumentNode
subscribeMutation =
  gql
    """
mutation updateCurrentUser($input: UserInput!) {
  update_user(input: $input) {
    user {
      id
      appleReceipt
      isSubscribed
      isPermitted
      isGuest
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
  androidMutationFn /\ r1 <- useMutation subscribeMutation {}
  iosMutationFn /\ r2 <- useMutation subscribeMutation {}
  { setLoading, setError } <- useContext dataStateContext
  queryResult <- useUserBooks {fetchPolicy: "cache-and-network"}
  let isGuest = fromMaybe true do
        r <- queryResult.state
        pure $ r.currentUser.isGuest
  useEffect unit do
     purchaseErrorListener $ \e -> launchAff_ do
        liftEffect $ log $ "ERROR: " <> show e
     purchaseUpdatedListener $ \p -> launchAff_ do
        if (isJust $ toMaybe $ p.transactionReceipt) && not isGuest then do
            let iosPayload = {variables: {input: {appleReceipt: p.transactionReceipt} }}
            let androidPayload = {variables: {input: {androidReceipt: p.transactionReceipt}}}
            result <- try $ Platform.select {
                ios: iosMutationFn $ iosPayload,
                android: androidMutationFn $ androidPayload
              }
            case result of
                 Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
                 Right r -> do
                    _ <- track "Subscribe" {productId: "io.unchart.sub", quantity: 1, price: 11.99, revenueType: "income"}
                    finishTransaction p
        else mempty

     pure mempty

  mutationFn /\ result <- useMutation mutation {}
  pure $ M.getJsx $ traverse_ (authOrApp (dismiss mutationFn)) (spy "RESULT" queryResult).state

authOrApp :: Effect Unit -> User -> M.Markup Unit
authOrApp onDismiss d
  | d.currentUser.isGuest = authenticationNavigator {}
  | (fromMaybe true $ toMaybe d.currentUser.showPayment) && not d.currentUser.isSubscribed =
      portal {} $ M.childElement Subscribe.reactComponent {visible: true, onDismiss: onDismiss}
  | not $ isCurrentVersion d = M.childElement Introduction.reactComponent {}
  | otherwise = tabNavigator {}
