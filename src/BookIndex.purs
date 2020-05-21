module BookIndex where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\))
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Effect.Aff (launchAff_)
import QueryHooks (useUserBooks, Book, User, useData)
import Paper (textInput, surface, button, listSection, listItem, listIcon)
import Markup as M
import Data.Maybe (Maybe(..), isJust)
import Record (merge)
import Data.Foldable (foldl)
import Record.Unsafe.Union (unsafeUnion)
import FS (readDirectory, File, bookDir, exists)
import React.Basic.Native.Events as RNE
import Effect.Class (liftEffect)
import Data.Foldable (find)
import Navigation (useFocusEffect)

type Props
  = { navigation :: { navigate :: EffectFn2 String { slug :: String } Unit } }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "BookIndex") buildJsx

buildJsx props = React.do
  files /\ setFiles <- useState (Nothing :: Maybe (Array File))
  useFocusEffect unit do
    launchAff_
      $ do
          doesExist <- exists bookDir
          files <- if doesExist then readDirectory bookDir else pure []
          liftEffect $ setFiles \_ -> Just files
    pure mempty
  queryResult <- useUserBooks {fetchPolicy: "cache-and-network"}
  case queryResult.state of
    Nothing -> pure mempty
    Just d -> dom d files
  where
  redirect slug = runEffectFn2 props.navigation.navigate "Read" { slug: slug }

  bookIcon :: forall p. Record p -> JSX
  bookIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "book" }

  cloudState book Nothing p = mempty

  cloudState book (Just files) p = element listIcon $ unsafeUnion p { color: "#000", icon: icon }
    where
    icon
      | isJust $ find (\f -> f.name == book.filename || f.name == book.slug) files = "check-bold"
      | otherwise = "cloud-outline"

  item :: Maybe (Array File) -> M.Markup Unit -> Book -> M.Markup Unit
  item files accum book =
    accum
      <> ( listItem
            { title: RN.string book.name
            , left: bookIcon
            , right: cloudState book files
            , onPress: RNE.capture_ $ redirect book.slug
            }
        )

  dom d files = React.do
    pure $ M.getJsx
      $ surface { style: M.css { flex: 1 } } do
         M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
            M.scrollView {style: M.css { flex: 1}} do
              listSection {} do
                foldl (item files) mempty d.currentUser.books
