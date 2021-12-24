module WiktionaryModal where
import React.Basic.Hooks as React
import Prelude
import Paper (surface, title, divider, button, modal, subheading, headline)
import React.Basic.Hooks (JSX, ReactComponent, useState, (/\), useEffect, useContext)
import Markup as M
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Effect (Effect)
import Effect.Unsafe (unsafePerformEffect)
import HTMLView (htmlView)
import Wiktionary (getDefinition, WiktionaryResult)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Effect.Aff (Aff, launchAff_, delay, forkAff, Milliseconds(..), try)
import Data.String (length, stripSuffix, stripPrefix, Pattern(..), toLower)
import Data.Map (fromFoldable, lookup) as Map
import Data.Interpolate (i)
import Data.Foldable (foldl)
import Control.Alt ((<|>))
import Effect.Class (liftEffect)
import React.Basic.Native.Events as RNE

languageList = Map.fromFoldable [
  ("en" /\ "English"),
  ("es" /\ "Spanish"),
  ("fr" /\ "French"),
  ("ru" /\ "Russian"),
  ("de" /\ "German"),
  ("it" /\ "Italian")
]
type Props = {word :: String, visible :: Boolean, onDismiss :: Effect Unit, language :: String}

surfaceStyle = M.css
  {
    borderRadius: 10,
    backgroundColor: "white",
    alignItems: "center",
    width: "100%",
    padding: 20,
    height: "80%"
  }

modalStyle = M.css
  {
    paddingTop: 0,
    margingTop: 0,
    paddingLeft: "4%",
    width: "95%"
  }


reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        React.reactComponent "WiktionaryModal" $ buildJsx

buildJsx props = React.do
  term /\ setTerm <- useState (Nothing :: Maybe String)
  wiktionaryEntry /\ setWiktionaryEntry <- useState $ (Nothing :: Maybe String)
  let dismiss = props.onDismiss
  useEffect (term /\ props.language /\ props.word) do
    launchAff_ do
      result <- wiktionary (term <|> (Just props.word)) props.language $ Just "en"
      liftEffect $ setWiktionaryEntry \_ -> Just $ dictHtml $ result
    pure mempty

  pure $ M.getJsx $
    case wiktionaryEntry of
         Nothing -> pure mempty
         Just d ->
            modal {visible: props.visible, contentContainerStyle: modalStyle, onDismiss: dismiss, dismissable: true} do
              M.view {style: surfaceStyle} do
                title {style: M.css {textAlign: "center"}} $ M.string $ fromMaybe "" $ term <|> (Just props.word)
                htmlView {value: d, onLinkPress: mkEffectFn1 \url -> (setTerm \_ -> wordFromUrl url props.language), stylesheet: {h4: {textAlign: "center"}}}
                button {onPress: RNE.capture_ $ dismiss, style: M.css {bottom: 5, position: "absolute"}} $ M.string "Exit"
  where wiktionary (Just text) language (Just locale) = getDefinition (toLower text) locale language
        wiktionary _ _ _ = pure []
        wordFromUrl url language = do
           prefixless <- stripPrefix (Pattern "/wiki/") $ url
           wikiLanguage <- Map.lookup language languageList
           stripSuffix (Pattern $ i "#" wikiLanguage) prefixless
        dictHtml :: WiktionaryResult -> String
        dictHtml d = foldl foldDefinitions "" d
        foldDefinitions accum d = i accum "<h4>" d.partOfSpeech "</h4>" "<ol>" (foldl foldDefinition "" d.definitions) "</ol>"
        foldDefinition accum d = i accum "<li>" d.definition "</li>"

