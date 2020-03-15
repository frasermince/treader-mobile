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

titleStyles =
  M.css
    { fontSize: 15
    , fontWeight: "bold"
    , marginBottom: 5
    }

type Payload = {variables :: {input :: {snippet :: String, language :: String}}}
type Props = {snippet :: Maybe String, labelText :: String, mutationFn :: Payload -> Aff {translate :: {translation :: String}} , language :: Maybe String}

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
  let translationText = (M.text {} <$> M.string <$> translation)
  let translationElement = (append translationMarker) <$> translationText

  pure $ M.getJsx $
    M.touchableOpacity { style: M.css { marginTop: 10 }, onPress: capture_ $ press props.snippet props.language translation setTranslation setShowTranslation } do
       fromMaybe mempty $ if showTranslation then (translationElement <|> textElement) else textElement
  where

  textElement = (append marker) <$> sentenceText

  translationMarker = M.text { style: titleStyles } $ M.string $ props.labelText <> " Translation"

  marker = M.text { style: titleStyles } $ M.string $ props.labelText <> " (tap to translate)"

  sentenceText = (M.text {} <$> M.string <$> props.snippet)


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


