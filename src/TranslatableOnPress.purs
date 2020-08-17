module TranslatableOnPress where
import Prelude
import Markup as M
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..), try)
import Data.Either (Either(..))
import Effect.Class (liftEffect)
import React.Basic.Native.Events (capture_)
import Control.Alt ((<|>))
import Effect.Unsafe (unsafePerformEffect)
import Debug.Trace (spy)
import Effect.Console (log)
import Paper (listItem, listIcon)
import Record.Unsafe.Union (unsafeUnion)

titleStyles =
  M.css
    { fontSize: 15
    , fontWeight: "bold"
    , marginBottom: 5
    }

descriptionStyle = M.css {color: "black", fontWeight: "400"}

type Payload = {variables :: {input :: {snippet :: String, language :: String}}}
type Props = {snippet :: Maybe String, labelText :: String, mutationFn :: Payload -> Aff {translate :: {translation :: String}} , language :: Maybe String}

translateIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "google-translate" }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "TranslatableOnPress") buildJsx

buildJsx props = React.do
  translation /\ setTranslation <- useState $ (Nothing :: Maybe String)
  showTranslation /\ setShowTranslation <- useState true
  useEffect props.snippet do
    setTranslation \_ -> Nothing
    setShowTranslation \_ -> true
    pure mempty
  let translationElement = do
        t <- translation
        pure $ listItem
          { titleStyle: titleStyles
          , style: M.css {paddingLeft: 0, marginLeft: 0}
          , descriptionStyle: descriptionStyle
          , title: props.labelText <> " Translation"
          , description: t
          , right: translateIcon
          , descriptionNumberOfLines: 5
          }

  pure $ M.getJsx $
    M.touchableOpacity { style: M.css { paddingTop: 2, borderTopColor: "#b2b2b2", borderTopWidth: 1 }, onPress: capture_ $ press props.snippet props.language translation setTranslation setShowTranslation } do
       fromMaybe mempty $ if showTranslation then (translationElement <|> textElement) else textElement
  where

  textElement = do
     s <- props.snippet
     pure $ listItem
       { titleStyle: titleStyles
       , style: M.css {paddingLeft: 0, marginLeft: 0}
       , title: props.labelText
       , description: s
       , descriptionStyle: descriptionStyle
       , right: translateIcon
       , descriptionNumberOfLines: 5
       }

  press (Just snippet) (Just language) Nothing setTranslation _ =
    launchAff_
      $ do
          let
            payload = { variables: { input: { snippet: snippet, language: language } } }
          response <- try $ props.mutationFn payload
          case response of
            Left err -> pure unit
            Right result -> do
              liftEffect $ setTranslation \_ -> Just $ result.translate.translation

  press _ _ (Just translation) _ setShowTranslation = do
    setShowTranslation \t -> not t

  press _ _ _ _ _ = do
    pure unit


