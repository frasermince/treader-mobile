module BookIndex where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\))
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Effect.Aff (launchAff_)
import QueryHooks (useUserBooks, Book, User, useData)
import Paper (textInput, surface, button, listSection, listItem, listIcon, fab, portal, modal, title)
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
import Linking (openUrl)
import Subscribe as Subscribe
import Data.Interpolate (i)
import Effect (Effect)

type Props
  = { navigation :: { navigate :: EffectFn2 String { slug :: String } Unit } }


actionBySubscription true _ setUploadVisible = setUploadVisible \_ -> true
actionBySubscription false setModalVisible _ = setModalVisible \_ -> true

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "BookIndex") buildJsx

buildJsx props = React.do
  files /\ setFiles <- useState (Nothing :: Maybe (Array File))

  modalVisible /\ setModalVisible <- useState false
  uploadVisible /\ setUploadVisible <- useState false
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
    Just d -> pure $ M.getJsx
      do
        portal {} $ M.childElement Subscribe.reactComponent {visible: modalVisible, onDismiss: setModalVisible \_ -> false}
        M.childElement uploadModal {uploadVisible, setUploadVisible}
        surface { style: M.css { flex: 1 } } do
          M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
              M.scrollView {style: M.css { flex: 1}} do
                listSection {} do
                  foldl (item files) mempty d.currentUser.books
              fab {icon: "plus", small: true, style: M.css {width: 40, position: "absolute", right: 5, bottom: 5}, onPress: RNE.capture_ $ actionBySubscription d.currentUser.isSubscribed setModalVisible setUploadVisible}
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
