module BookIndex where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\))
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1, EffectFn2, runEffectFn2)
import Effect (Effect)
import Effect.Aff (launchAff_)
import QueryHooks (useUserBooks, Book, User, useData)
import Paper (textInput, surface, button, listSection, listItem, listIcon)
import Markup as M
import Data.Maybe (Maybe(..), isJust)
import Record (merge)
import Data.Foldable (foldl)
import Record.Unsafe.Union (unsafeUnion)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import FS (readDirectory, File, bookDir, exists)
import React.Basic.Native.Events as RNE
import Effect.Class (liftEffect)
import Data.Foldable (find)
import Navigation (useFocusEffect)
import Effect.Console (log)
import Debug.Trace (spy)

type JSProps
  = { navigation :: { navigate :: EffectFn2 String { slug :: String } Unit, addListener ::
            EffectFn2 String
              ( EffectFn1
                  { preventDefault :: Effect Unit
                  }
                  Unit
              )
              (Effect Unit)
}}

type Props
  = { navigation ::
        { addListener ::
            String ->
              ( { preventDefault :: Effect Unit } ->
              Effect Unit
            ) ->
            Effect (Effect Unit),
            navigate :: String -> { slug :: String } -> Effect Unit
        }
    }


convertProps :: JSProps -> Props
convertProps props =
  {
   navigation: {
      addListener: \s f -> runEffectFn2 (spy "ADD LISTENER" props.navigation.addListener) s (mkEffectFn1 f),
      navigate: runEffectFn2 props.navigation.navigate
    }
  }
reactComponent :: ReactComponent JSProps
reactComponent =
  unsafePerformEffect
    $ do
        (component "SignIn") buildJsx

buildJsx jsProps = React.do
  files /\ setFiles <- useState (Nothing :: Maybe (Array File))

  useEffect unit do
    log "FOCUS"
    unsubscribe <- props.navigation.addListener "tabPress" \e -> do
       log "HI"
       e.preventDefault
    pure $ unsubscribe

  useFocusEffect unit do
    launchAff_
      $ do
          doesExist <- exists bookDir
          files <- if doesExist then readDirectory bookDir else pure []
          liftEffect $ setFiles \_ -> Just files
    pure mempty
  queryResult <- useUserBooks {}
  case queryResult of
    Nothing -> pure mempty
    Just d -> dom d files
  where

  props = convertProps jsProps
  redirect slug = props.navigation.navigate "Read" { slug: slug }

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
      $ M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface { style: M.css { flex: 1 } } do
            listSection {} do
              foldl (item files) mempty d.currentUser.books
