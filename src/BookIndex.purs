module BookIndex where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\))
import Effect.Uncurried (runEffectFn1, EffectFn1)
import QueryHooks (useUserBooks, Book, User, useData)
import Paper (textInput, surface, button, listSection, listItem, listIcon)
import Markup as M
import Data.Maybe (Maybe(..))
import Record (merge)
import Data.Foldable (foldl)
import Record.Unsafe.Union (unsafeUnion)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import React.Basic.Native.Events as RNE
type Props
  = {navigation :: {navigate :: EffectFn2 String {slug :: String} Unit}}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "SignIn") buildJsx

buildJsx props = React.do
  queryResult <- useUserBooks {}
  case queryResult of
       Nothing -> pure mempty
       Just d -> dom d

  where
    redirect slug = runEffectFn2 props.navigation.navigate "Book" {slug: slug}
    icon :: forall p . Record p -> JSX
    icon p = element listIcon $ unsafeUnion p {color: "#000", icon: "book"}
    item :: M.Markup Unit -> Book -> M.Markup Unit
    item accum book = accum <> (listItem {title: RN.string book.name, left: icon, onPress: RNE.capture_ $ redirect book.slug})
    dom d = React.do
      pure $ M.getJsx $ surface {style: M.css {flex: 1}} do
        listSection {} do
          foldl item mempty d.currentUser.books

