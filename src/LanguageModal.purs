module LanguageModal where
import Prelude
import Effect (Effect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\))
import React.Basic.Hooks as React
import Paper (textInput, surface, button, listSection, listItem, listIcon, divider, title, portal, dialog, dialogTitle, dialogContent, dialogActions)
import Markup as M
import Data.Maybe (Maybe(..), fromMaybe)
import React.Basic.Native.Events as RNE
import Effect.Unsafe (unsafePerformEffect)

type Props =
  { visible :: Boolean
  , setVisible :: (Boolean -> Boolean) -> Effect Unit
  , setLanguage :: (Maybe String -> Maybe String) -> Effect Unit
  , language :: Maybe String
  }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (React.reactComponent "LanguageModal") buildJsx

selectableItem language setLanguage value label dismiss = listItem {title: label, onPress: RNE.capture_ $ select}
  where select = do
          setLanguage \_ -> value
          dismiss

buildJsx props = React.do
  let dismiss = props.setVisible \_ -> false
  pure $ M.getJsx $ portal {} $ dialog {visible: props.visible, onDismiss: dismiss} do
    dialogTitle {} $ M.string "Choose Language"
    dialogContent {style: M.css {height: 280}} do
        selectableItem props.language props.setLanguage (Just "en") "English" dismiss
        selectableItem props.language props.setLanguage (Just "fr") "French" dismiss
        selectableItem props.language props.setLanguage (Just "de") "German" dismiss
        selectableItem props.language props.setLanguage (Just "it") "Italian" dismiss
        selectableItem props.language props.setLanguage (Just "ru") "Russian" dismiss
        selectableItem props.language props.setLanguage (Just "es") "Spanish" dismiss
    dialogActions {} do
        button {onPress: RNE.capture_ $ dismiss} $ M.string "Cancel"

