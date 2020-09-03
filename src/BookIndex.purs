module BookIndex where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\), useLayoutEffect)
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Effect.Aff (launchAff_)
import QueryHooks (useUserBooks, Book, User, useData)
import Paper (textInput, surface, button, listSection, listItem, listIcon, fab, portal, modal, title, dialog, dialogTitle, dialogContent, dialogActions, radioButton)
import Markup as M
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Record (merge)
import Data.Foldable (foldl)
import Record.Unsafe.Union (unsafeUnion)
import FS (readDirectory, File, bookDir, exists)
import React.Basic.Native.Events as RNE
import Effect.Class (liftEffect)
import Data.Foldable (find)
import Navigation (useFocusEffect)
import Linking (openUrl)
import Subscribe as Subscribe
import Data.Interpolate (i)
import Effect (Effect)
import Global (encodeURIComponent)

type Props
  = { navigation :: { navigate :: EffectFn2 String { slug :: String } Unit, setOptions :: EffectFn1 {headerRight :: ReactComponent {}} Unit } }


actionBySubscription true _ setUploadVisible = setUploadVisible \_ -> true
actionBySubscription false setModalVisible _ = setModalVisible \_ -> true

buttonComponent :: ((Boolean -> Boolean) -> Effect Unit) -> Maybe String -> ReactComponent {}
buttonComponent setLanguageModalVisible language = unsafePerformEffect
    $ do
        (component "TopButton") $ buttonJsx setLanguageModalVisible language

buttonJsx setLanguageModalVisible language props = React.do
  pure $ M.getJsx $ button {onPress: RNE.capture_ $ setLanguageModalVisible \_ -> true, style: M.css {position: "absolute", right: 1, marginTop: 21}} $ M.string $ fromMaybe "Language" $ language

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "BookIndex") buildJsx

selectableItem language setLanguage value label dismiss = listItem {title: label, onPress: RNE.capture_ $ select}
  where select = do
          setLanguage \_ -> value
          dismiss

buildJsx props = React.do
  files /\ setFiles <- useState (Nothing :: Maybe (Array File))
  language /\ setLanguage <- useState (Nothing :: Maybe String)

  modalVisible /\ setModalVisible <- useState false
  languageModalVisible /\ setLanguageModalVisible <- useState false
  uploadVisible /\ setUploadVisible <- useState false
  let dismiss = setLanguageModalVisible \_ -> false
  useLayoutEffect language do
     runEffectFn1 props.navigation.setOptions {headerRight: buttonComponent setLanguageModalVisible language}
     pure mempty
  useFocusEffect unit do
    launchAff_
      $ do
          doesExist <- exists bookDir
          files <- if doesExist then readDirectory bookDir else pure []
          liftEffect $ setFiles \_ -> Just files
    pure mempty
  queryResult <- useUserBooks {fetchPolicy: "cache-and-network"}

  useEffect queryResult.state do
     setLanguage \_ -> do
        q <- queryResult.state
        pure $ q.currentUser.language
     pure mempty

  case queryResult.state of
    Nothing -> pure mempty
    Just d -> pure $ M.getJsx
      do
        portal {} $ dialog {visible: languageModalVisible, onDismiss: setLanguageModalVisible \_ -> false} do
           dialogTitle {} $ M.string "Choose Language"
           dialogContent {style: M.css {height: 340}} do
              selectableItem language setLanguage (Just "fr") "French" dismiss
              selectableItem language setLanguage (Just "es") "Spanish" dismiss
              selectableItem language setLanguage (Just "it") "Italian" dismiss
              selectableItem language setLanguage (Just "de") "German" dismiss
              selectableItem language setLanguage (Just "ru") "Russian" dismiss
              selectableItem language setLanguage (Just "en") "English" dismiss
              selectableItem language setLanguage Nothing "All Books" dismiss
           dialogActions {} do
              button {onPress: RNE.capture_ $ dismiss} $ M.string "Cancel"
        portal {} $ M.childElement Subscribe.reactComponent {visible: modalVisible, onDismiss: setModalVisible \_ -> false}
        M.childElement uploadModal {uploadVisible, setUploadVisible}
        surface { style: M.css { flex: 1 } } do
          M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
              M.view {style: M.css { flex: 1}} do
                M.scrollView {} do
                  listSection {} do
                    foldl (item language files) mempty d.currentUser.books
              fab {icon: "plus", small: true, style: M.css {width: 40, position: "absolute", right: 5, bottom: 5}, onPress: RNE.capture_ $ actionBySubscription d.currentUser.isSubscribed setModalVisible setUploadVisible}
  where
  redirect slug = runEffectFn2 props.navigation.navigate "Read" { slug: slug }

  bookIcon :: forall p. Boolean -> Record p -> JSX
  bookIcon hasAudio p = element listIcon $ unsafeUnion p { color: "#000", icon: if hasAudio then "book-music" else "book" }

  cloudState book Nothing p = mempty

  cloudState book (Just files) p = element listIcon $ unsafeUnion p { color: "#000", icon: icon }
    where
    icon
      | isJust $ find (\f -> f.name == (fromMaybe book.filename $ encodeURIComponent book.filename) || f.name == book.slug) files = "check-bold"
      | otherwise = "cloud-outline"

  item :: Maybe String -> Maybe (Array File) -> M.Markup Unit -> Book -> M.Markup Unit
  item language files accum book =
    case language of
         Nothing -> newList book
         Just l -> if book.language == l then newList book else accum

    where newList book = accum
            <> ( listItem
                  { title: RN.string book.name
                  , left: bookIcon (book.audioChapters /= [])
                  , right: cloudState book files
                  , onPress: RNE.capture_ $ redirect book.slug
                  }
        )

type UploadProps = {setUploadVisible :: (Boolean -> Boolean) -> Effect Unit, uploadVisible :: Boolean}
uploadModal :: ReactComponent UploadProps
uploadModal =
  unsafePerformEffect
    $ do
        (component "UploadModal") uploadModalDom

uploadModalDom props = React.do
  pure $ M.getJsx $
    portal {} $ modal {visible: props.uploadVisible, contentContainerStyle: modalStyle, onDismiss: props.setUploadVisible \_ -> false} do
      M.view {style: surfaceStyle} do
        M.view {style: M.css {flex: 6, width: "100%", alignItems: "center"}} do
          title {} $ M.string "Upload"
          numberedItem "Click on the button below to be redirected to the web interface." 1
          numberedItem "Login to your account and you will see a button for book uploads." 2
          numberedItem "Click the button and choose a DRM free epub file." 3
          numberedItem "After uploading it may take a couple of hours to see your book." 4
        M.view {style: M.css {flex: 1, width: "100%", alignItems: "center"}} do
          button {style: mainButtonStyle, onPress: RNE.capture_ $ openUrl "https:/app.unchart.io", mode: "contained"} $ M.string "Continue"
          M.text {style: M.css {textAlign: "center"}, onPress: RNE.capture_ $ props.setUploadVisible \_ -> false} $ M.string "Close"
      M.view {style: bottomViewStyle} $ mempty

numberedItem text number = listItem
  { title: RN.string text
  , left: numberIcon number
  , style: M.css {width: "95%", marginRight: 20, marginLeft: 20}
  , titleNumberOfLines: 3
  }

numberIcon :: forall p. Int -> Record p -> JSX
numberIcon number p = element listIcon $ unsafeUnion p { color: "#000", icon: "numeric-" <> show number <> "-box"}

mainButtonStyle = M.css
  {
    marginBottom: 15,
    width: 300,
    height: 40,
    textSize: 50
  }

surfaceStyle = M.css
  {
    paddingTop: 20,
    borderRadius: 10,
    backgroundColor: "white",
    alignItems: "center",
    width: "100%",
    flex: 4
  }

modalStyle = M.css
  {
    paddingTop: 0,
    marginTop: 0,
    paddingLeft: "4%",
    width: "95%",
    height: "100%"
  }

bottomViewStyle = M.css
  {
    marginTop: 20,
    alignItems: "center",
    flex: 1
  }
