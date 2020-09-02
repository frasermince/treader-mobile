module Subscribe where

import Prelude
import React.Basic.Hooks as React
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import Paper (surface, title, divider, button, modal, subheading, headline, listItem, listIcon)
import Record.Unsafe.Union (unsafeUnion)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Markup as M
import Swiper (swiper)
import React.Basic.Native.Events as RNE
import Effect (Effect)
import InAppPurchases (requestSubscription, getSubscriptions, purchaseUpdatedListener, purchaseErrorListener, finishTransaction)
import Effect.Aff (Aff, launchAff_, try)
import Effect.Class (liftEffect)
import Context (dataStateContext, Context)
import Effect.Console (log)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Effect.Exception (message)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)
import Data.Array (head)
import Data.Either (Either(..))
import ApolloHooks (useMutation, gql, DocumentNode)
import Data.Nullable (toMaybe, Nullable)
import Data.String (stripPrefix, Pattern(..))
import Platform as Platform
import Segment (track, screen)

type Props = {visible :: Boolean, onDismiss :: Effect Unit}


reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Subscribe" $ buildJsx

purchaseHandler 0 setError = launchAff_ do
  subs <- getSubscriptions ["io.unchart.premium"]
  let sku = _.productId <$> (head subs)
  purchase sku setError

purchaseHandler 1 setError = launchAff_ do
  subs <- getSubscriptions ["io.unchart.plus"]
  let sku = _.productId <$> (head subs)
  purchase sku setError

purchaseHandler _ setError = liftEffect $ runEffectFn1 setError "Product not found"

purchase Nothing _ = mempty
purchase (Just sku) setError = do
     result <- try $ requestSubscription sku false
     case result of
          Left error -> liftEffect $ runEffectFn1 setError $ message error
          Right resp -> liftEffect $ (log $ "PURCHASE RESPONSE " <> (show resp))

translateIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "google-translate" }
bookIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "book" }
plusIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "plus" }

buildJsx props = React.do
  { setLoading, setError } <- useContext dataStateContext
  swipeIndex /\ setSwipeIndex <- useState 0
  useEffect unit do
     purchaseUpdatedListener $ \p -> launchAff_ do
        if isJust $ toMaybe $  p.transactionReceipt then do
          liftEffect $ props.onDismiss
          else mempty
     pure mempty

  pure $ M.getJsx do
      let dismiss = props.onDismiss
      modal {visible: props.visible, contentContainerStyle: modalStyle, onDismiss: dismiss} do
          M.view {style: surfaceStyle} do
            M.view {style: benefitsSectionStyle} do
              swiper
                { nextButton: M.getJsx $ M.text {style: M.css {color: "#66aab1", fontSize: 40}} $ M.string "›"
                , prevButton: M.getJsx $ M.text {style: M.css {color: "#66aab1", fontSize: 40 }} $ M.string "‹"
                , showsButtons: true
                , style: M.css {}
                , horizontal: true
                , autoplay: false
                , showButtons: true
                , index: swipeIndex
                , onIndexChanged: mkEffectFn1 \index -> setSwipeIndex \_ -> index
                , loop: false
                } do
                  M.view {style: slideStyle} do
                     title {style: M.css {textSize: 30}} $ M.string "Upgrade to Unchart Plus for"
                     M.text {style: priceStyle} $ M.string "$6.99/mo"
                  M.view {style: slideStyle} do
                     title {style: M.css {textSize: 30}} $ M.string "Upgrade to Unchart Premium for"
                     M.text {style: priceStyle} $ M.string "$11.99/mo"

            M.view {style: priceSectionStyle} do
              listItem {style: M.css {width: "100%", marginLeft: "20%"}, titleStyle: M.css {color: "black"}, title: RN.string "Translate Unlimited Words", left: translateIcon}
              listItem {style: M.css {width: "100%", marginLeft: "20%"}, titleStyle: M.css {color: "black"}, title: RN.string "Upload Your Own Epub Books", left: bookIcon}
              if swipeIndex == 1 then listItem {style: M.css {width: "100%", marginLeft: "20%"}, titleStyle: M.css {color: "black"}, title: RN.string "Create Unlimited Flashcards", left: plusIcon} else mempty
              M.view {style: bottomStyle} do
                button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ purchaseHandler swipeIndex setError} $ M.string $ "SUBSCRIBE FOR " <> if swipeIndex == 0 then "$6.99" else "$11.99"
                button {onPress: RNE.capture_ $ dismiss} $ M.string "NO THANKS"
          M.view {style: bottomViewStyle} do
              M.text {style: M.css {color: "white"}} $ M.string "By tapping the subscribe button you are enrolling in automatic payments of the listed amount that will continue until you cancel."

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
    marginTop: "10%",
    fontWeight: "700",
    fontSize: 28
  }

textStyle = M.css
  {
    color: "black"
  }
