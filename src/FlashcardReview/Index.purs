module FlashcardReview.Index where

import Prelude

import ApolloHooks (useMutation, gql)
import ComponentTypes (Flashcard)
import Control.Alt ((<|>))
import Data.Array (length)
import Data.Array.NonEmpty (fromArray, toArray, NonEmptyArray)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Data.Traversable (traverse_)
import Debug.Trace (spy)
import Effect.Console (log)
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Effect.Unsafe (unsafePerformEffect)
import Icon (materialCommunityIcon)
import LanguageModal as LanguageModal
import Markup as M
import Navigation (useFocusEffect)
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon, subheading)
import QueryHooks (useData, UseData, stripGraphqlError)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import React.Basic.Hooks as React
import React.Basic.Native.Events as RNE
import React.Basic.Native.Events as RNE
import Type.Proxy (Proxy(..))

mainButtonStyle = M.css
  {
    marginBottom: 15,
    width: 300,
    height: 40,
    textSize: 50
  }

type Props
  = { navigation :: { navigate :: EffectFn2 String {existingIds :: Maybe (Array String), flashcards :: Maybe (NonEmptyArray Flashcard) } Unit}, route :: { params :: { complete :: Boolean }}}

type Query = {flashcards :: Array Flashcard, currentUser :: {currentReview :: Nullable (Array Int), language :: String}}

query =
  gql
    """
    query getFlashcards($language: String) {
      currentUser {
        id
        currentReview
        language
      }
      flashcards(language: $language) {
        id
        imageUrl
        a
        b
        t
        startOffset
        word
        hoursPassed
        sentence {
          id
          audioUrl
          text
          translation
        }
      }
    }
  """

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "ReviewIndex" $ buildJsx

currentReviewMaybe Nothing = Nothing
currentReviewMaybe (Just d) = Just $ d {currentUser {currentReview = toMaybe d.currentUser.currentReview}}

buildJsx props = React.do
  flashcardsResult <- useData (Proxy :: Proxy Query) query {errorPolicy: "all", fetchPolicy: "network" }
  languageModalVisible /\ setLanguageModalVisible <- useState false
  language /\ setLanguage <- useState (Nothing :: Maybe String)
  let currentLanguage = do
        r <- flashcardsResult.state
        language <|> (Just r.currentUser.language)

  useEffect language do
     traverse_ (\l -> flashcardsResult.refetch {language: l}) language
     pure mempty

  useFocusEffect unit do
     traverse_ (\l -> flashcardsResult.refetch {language: l}) currentLanguage
     pure mempty

  case currentReviewMaybe flashcardsResult.state of
       Nothing -> mempty
       Just {flashcards: []} -> pure $ M.getJsx $
          M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
            M.view {style: M.css {alignItems: "center", height: "100%", marginTop: "55%", marginLeft: "10%", marginRight: "10%", textAlign: "center"}} do
              headline {style: M.css {marginBottom: 20, fontSize: 28}} $ M.string "Well this is empty..."
              subheading {style: M.css {textAlign: "center", lineHeight: 36, fontSize: 20, flexDirection: "row", flexWrap: "wrap", justifyContent: "space-between", alignItems: "center", flexShrink: 1}} $ do
                 M.text {style: M.css {}} $ M.string "Review flashcards to boost your memory. Tap on the "
                 materialCommunityIcon { name: "card-bulleted-outline", size: 32, style: M.css { marginTop: 10} }
                 M.text {style: M.css {}} $ M.string "  icon to create flashcards and then review them here."

       Just {flashcards, currentUser: {currentReview: Just currentReview}} ->
         pure $ M.getJsx do
          M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
              title {style: M.css {marginTop: "20%", marginLeft: "5%", marginRight: "5%", textAlign: "center", flex: 2}} $ M.string $ "Finish the " <> (show $ length currentReview) <> " flashcards in your current review"
              M.view {style: M.css {flex: 3, alignItems: "center"}} do
                button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ runEffectFn2 (spy "PROPS: " props).navigation.navigate "Review" {existingIds: Just $ map show currentReview, flashcards: fromArray flashcards}} $ M.string $ "Complete Review"
       Just {flashcards} ->
         pure $ M.getJsx do
           M.childElement LanguageModal.reactComponent
             { visible: languageModalVisible
             , setVisible: setLanguageModalVisible
             , language: currentLanguage
             , setLanguage: setLanguage
             }

           M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
            title {style: M.css {marginTop: "10%", textAlign: "center", flex: 2}} $ M.string $ if props.route.params.complete then "Review complete!" else "Start a review of " <> (show $ min 30 (length $ spy "***FLASHCARDS" flashcards)) <> " flashcards"
            M.view {style: M.css {flex: 3, alignItems: "center"}} do
              button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ runEffectFn2 props.navigation.navigate "Review" {existingIds: Nothing, flashcards: fromArray flashcards}} $ M.string $ if props.route.params.complete then "Review MOre" else "Start Review"
            button {onPress: RNE.capture_ $ setLanguageModalVisible \_ -> true, style: M.css {position: "absolute", bottom: 5, right: 2}} $ M.string $ fromMaybe "" $ currentLanguage
