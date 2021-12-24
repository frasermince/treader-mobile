module Account where

import Prelude
import React.Basic.Native as RN
import Effect.Aff (launchAff_)
import React.Basic.Native.Events as RNE
import React.Basic.Hooks as React
import Effect.Unsafe (unsafePerformEffect)
import AsyncStorage (clear, getItem, removeItem)
import Effect.Class (liftEffect)
import Paper (surface, title, divider, button, modal, subheading, headline, listItem, listIcon, listSection, portal)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Record.Unsafe.Union (unsafeUnion)
import Markup as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import ApolloHooks (useMutation, gql, DocumentNode, useApolloClient)
import QueryHooks (useUserBooks)
import Data.Nullable (toMaybe, Nullable)
import Subscribe as Subscribe
import Data.DateTime.Instant (instant, toDateTime)
import Data.Time.Duration (Milliseconds(..)) as Duration
import Data.DateTime (date, month, day, year)
import Data.Formatter.DateTime (format, FormatterCommand(..))
import Data.List (List(..), (:))
import Linking (openUrl, canOpenUrl)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)

type Props = {navigation :: { navigate :: EffectFn2 String {} Unit }}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        React.reactComponent "Account" $ buildJsx


buildJsx props = React.do
  client <- useApolloClient
  result <- useUserBooks {}
  modalVisible <- useState false
  pure $ M.getJsx $ jsxFromUser props.navigation.navigate result.state client modalVisible

signoutIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "logout" }

chevron p = element listIcon $ unsafeUnion p { color: "#000", icon: "chevron-right" }

logout client = launchAff_ do
  _ <- removeItem "treader-session"
  liftEffect $ traverse_ _.resetStore client

openSubscriptionPage = if canOpenUrl "https://apps.apple.com/account/subscriptions" then openUrl "https://apps.apple.com/account/subscriptions" else mempty

subscription true (Just subscriptionEndDate) _ = listItem {
  title: RN.string "Manage My Subscription",
  description: description subscriptionDate,
  right: chevron,
  onPress: RNE.capture_ openSubscriptionPage
}
  where description (Just d) = RN.string $ "1 Month Unchart Premium Membership. Cycle ends on " <> format (MonthFull : (Placeholder " ") : DayOfMonth : Nil) d
        description Nothing = mempty
        subscriptionDate = do
          inst <- instant $ Duration.Milliseconds $ subscriptionEndDate * 1000.0
          pure $ toDateTime $ inst

subscription true Nothing _ = listItem {title: RN.string "Manage My Subscription", right: chevron, onPress: RNE.capture_ $ openSubscriptionPage}
subscription false _ setModalVisible = listItem {title: RN.string "Upgrade Now", right: chevron, onPress: RNE.capture_ $ setModalVisible \_ -> true}

openPrivacyPolicy = openUrl "https://app.unchart.io/privacy-policy"
openTermsOfUse = openUrl "http://www.apple.com/legal/itunes/appstore/dev/stdeula"
openContact = openUrl "mailto:fraser@unchart.io"

jsxFromUser navigate (Just d) client (modalVisible /\ setModalVisible) = do
  portal {} $ M.childElement Subscribe.reactComponent {visible: modalVisible, onDismiss: setModalVisible \_ -> false}
  M.view {style: containerStyle} do
    title {style: M.css {marginBottom: 15}} $ M.string $ d.currentUser.firstName <> " " <> d.currentUser.lastName
    divider {style: M.css {height: 1, width: "100%"}}
    listSection {style: M.css {width: "100%"}} do
      subscription d.currentUser.isSubscribed (toMaybe d.currentUser.subscriptionEndDate) setModalVisible
      divider {style: M.css {height: 1, width: "100%"}}
      listItem {title: RN.string "Set Languages and Goals", onPress: RNE.capture_ $ runEffectFn2 navigate "LanguageSettings" {}, right: chevron}
      divider {style: M.css {height: 1, width: "100%"}}
      listItem {title: RN.string "Privacy Policy", onPress: RNE.capture_ $ openPrivacyPolicy, right: chevron}
      divider {style: M.css {height: 1, width: "100%"}}
      listItem {title: RN.string "Terms Of Use", onPress: RNE.capture_ $ openTermsOfUse, right: chevron}
      divider {style: M.css {height: 1, width: "100%"}}
      listItem {title: RN.string "Contact Us", onPress: RNE.capture_ $ openContact, right: chevron}

      divider {style: M.css {height: 1, width: "100%"}}
      listItem {title: RN.string "Signout", right: signoutIcon, onPress: RNE.capture_ $ logout client}
    divider {style: M.css {height: 1, width: "100%"}}
jsxFromUser _ Nothing _ _ = mempty

containerStyle = M.css {
  marginTop: 50,
  justifyContent: "center",
  alignItems: "center"
}

