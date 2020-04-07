module Subscribe where

import Prelude
import React.Basic.Hooks as React
import Effect.Unsafe (unsafePerformEffect)
import Paper (surface, title, divider, button, modal, subheading, headline)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext)
import Markup as M
import Swiper (swiper)
import React.Basic.Native.Events as RNE
import Effect (Effect)
import Icon (icon)
import InAppPurchases (requestSubscription, getSubscriptions, purchaseUpdatedListener, purchaseErrorListener, finishTransactionIOS)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Context (dataStateContext, Context)
import Effect.Console (log)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Effect.Exception (message)
import Effect.Uncurried (runEffectFn1, EffectFn1)
import Data.Array (head)
import Data.Either (Either(..))
import ApolloHooks (useMutation, gql, DocumentNode)
import Data.Nullable (toMaybe, Nullable)
import Data.String (stripPrefix, Pattern(..))

type Props = {visible :: Boolean, onDismiss :: Effect Unit}

mutation :: DocumentNode
mutation =
  gql
    """
mutation updateCurrentUser($input: UserInput!) {
  update_user(input: $input) {
    user {
      id
      appleReceipt
      isSubscribed
      isPermitted
    }
  }
}
  """

stripGraphqlError message = fromMaybe message $ stripPrefix (Pattern "GraphQL error: ") message

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Subscribe" $ buildJsx

purchaseHandler setError = launchAff_ do
  subs <- getSubscriptions ["io.unchart.sub"]
  let sku = _.productId <$> (head subs)
  purchase sku setError

purchase Nothing _ = mempty
purchase (Just sku) setError = do
     result <- try $ requestSubscription sku false
     case result of
          Left error -> liftEffect $ runEffectFn1 setError $ message error
          Right resp -> liftEffect $ (log $ "PURCHASE RESPONSE " <> (show resp))

buildJsx props = React.do
  mutationFn /\ result <- useMutation mutation {}
  { setLoading, setError } <- useContext dataStateContext
  useEffect unit do
     purchaseErrorListener $ \e -> launchAff_ do
        liftEffect $ log $ "ERROR: " <> show e
     purchaseUpdatedListener $ \p -> launchAff_ do
        if isJust $ toMaybe p.transactionReceipt then do
            result <- try $ mutationFn $ {variables: {input: {apple_receipt: p.transactionReceipt}}}
            case result of
                 Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
                 Right r -> do
                    finishTransactionIOS p.transactionId
                    liftEffect $ props.onDismiss
        else mempty

     pure mempty

  pure $ M.getJsx do
      let dismiss = props.onDismiss
      modal {visible: props.visible, contentContainerStyle: modalStyle, onDismiss: dismiss} do
          M.view {style: surfaceStyle} do
            M.view {style: benefitsSectionStyle} do
              title {} $ M.string "Upgrade to Unchart Premium"
              swiper {style: M.css {}, horizontal: true, autoplay: true, showButtons: true, loop: true} do
                  M.view {style: slideStyle} do
                     subheading {style: textStyle} $ M.string "Translate Unlimited Words"
                  M.view {style: slideStyle} $ subheading {style: textStyle} $ M.string "Upload Your Own Books"

            M.view {style: priceSectionStyle} do
              subheading {} $ M.string "Upgrade to Premium for"
              subheading {style: M.css {marginTop: "8%"}} $ M.string "30 days free and then"
              M.text {style: priceStyle} $ M.string "$11.99/mo"
              M.view {style: bottomStyle} do
                button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ purchaseHandler setError} $ M.string "START FREE TRIAL"
                button {onPress: RNE.capture_ $ dismiss} $ M.string "NO THANKS"
          M.view {style: bottomViewStyle} do
              M.text {style: M.css {color: "white"}} $ M.string "By tapping the start free trail subscription button you are enrolling in automatic payments of the listed amount, beginning at the end of your free trial that will continue until you cancel."

benefitsSectionStyle = M.css
  {
    justifyContent: "center",
    alignItems: "center",
    width: "100%",
    flex: 1,
    borderBottomColor: "b2b2b2",
    borderBottomWidth: 0.5,
    borderRadius: 10,
    paddingTop: 30
  }

priceSectionStyle = M.css
  {
    alignItems: "center",
    width: "100%",
    paddingTop: 15,
    flex: 1
  }

bottomViewStyle = M.css
  {
    marginTop: 20,
    alignItems: "center",
    flex: 1
  }

modalStyle = M.css
  {
    paddingTop: 0,
    margingTop: 0,
    paddingLeft: "4%",
    width: "95%",
    height: "100%"
  }

surfaceStyle = M.css
  {
    borderRadius: 10,
    backgroundColor: "white",
    justifyContent: "center",
    alignItems: "center",
    width: "100%",
    flex: 4
  }

slideStyle = M.css
  {
    justifyContent: "center",
    alignItems: "center",
    height: "100%"
  }

mainButtonStyle = M.css
  {
    marginBottom: 15,
    width: 300,
    height: 40,
    textSize: 50
  }

bottomStyle = M.css
  {
    flex: 1,
    justifyContent: "flex-end",
    marginBottom: 16
  }

priceStyle = M.css
  {
    marginTop: 10,
    fontWeight: "700",
    fontSize: 28
  }

textStyle = M.css
  {
    color: "black"
  }
