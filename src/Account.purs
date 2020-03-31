module Account where

import Prelude
import React.Basic.Native as RN
import Effect.Aff (launchAff_)
import React.Basic.Native.Events as RNE
import React.Basic.Hooks as React
import Effect.Unsafe (unsafePerformEffect)
import AsyncStorage (clear, getItem, removeItem)
import Effect.Class (liftEffect)
import Paper (surface, title, divider, button, modal, subheading, headline, listItem, listIcon, listSection)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Record.Unsafe.Union (unsafeUnion)
import Markup as M
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import ApolloHooks (useMutation, gql, DocumentNode, useApolloClient)
import QueryHooks (useUserBooks)

type Props = {}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Account" $ buildJsx


buildJsx props = React.do
  client <- useApolloClient
  maybeResult <- useUserBooks {}
  pure $ M.getJsx $ jsxFromUser maybeResult client

signoutIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "logout" }

logout client = launchAff_ do
  _ <- removeItem "treader-session"
  liftEffect $ traverse_ _.resetStore client

jsxFromUser (Just d) client = do
  M.view {style: containerStyle} do
    title {style: M.css {marginBottom: 15}} $ M.string $ d.currentUser.firstName <> " " <> d.currentUser.lastName
    divider {style: M.css {height: 1, width: "100%"}}
    listSection {style: M.css {width: "100%"}} do
      listItem {title: RN.string "Signout", right: signoutIcon, onPress: RNE.capture_ $ logout client}
    divider {style: M.css {height: 1, width: "100%"}}
jsxFromUser Nothing _ = mempty

containerStyle = M.css {
  marginTop: 50,
  justifyContent: "center",
  alignItems: "center"
}

