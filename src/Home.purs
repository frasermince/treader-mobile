module Home where
import Prelude
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Native as RN
import React.Basic.Native.Events as RNE
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\))
import Markup as M
import Paper (textInput, surface, button, listSection, listItem, listIcon, divider, title, portal, dialog, dialogTitle, dialogContent, dialogActions)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Record.Unsafe.Union (unsafeUnion)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Array (findIndex, (!!), find)
import ApolloHooks (useMutation, gql)
import Type.Proxy (Proxy(..))
import Navigation (useFocusEffect)
import FirebaseMessaging (requestPermission)
import Effect.Aff (Aff, launchAff_, try)
import Data.Interpolate (i)
import CefrLevels (levels)
import LanguageModal as LanguageModal

type Props = { navigation :: { navigate :: EffectFn2 String {} Unit } }

chevron p = element listIcon $ unsafeUnion p { color: "#000", icon: "chevron-right" }
type Query
  = { currentUser :: {dailyReviewedSessions :: Int, flashcardCountPerLanguage :: Array {count :: Int, language :: String}, currentStreak :: Int, dailyCreatedCards :: Int, dailyReadPages :: Int, id :: String, dailyGoal :: {pages :: Int, reviewSessions :: Int, created :: Int}, language :: String, startingLevel :: Int} }
query =
  gql
    """
query getUser {
  currentUser {
    language
    id
    dailyReviewedSessions
    dailyCreatedCards
    dailyReadPages
    startingLevel
    flashcardCountPerLanguage {
      count
      language
    }
    currentStreak
    dailyGoal {
      created
      pages
      reviewSessions
    }
  }
}
"""

topMetric title content =
  M.view {style: M.css {flex: 1}} do
    M.text {style: M.css {textAlign: "center", fontWeight: "bold"}} $ M.string title
    M.text {style: M.css {textAlign: "center"}} $ M.string content

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "Home") buildJsx

checkEmptyIcon total goal p
  | total >= goal = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-marked-outline" }
  | otherwise = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-blank-outline" }

currentLevelName flashcardCount startingLevel = fromMaybe "A0" do
  levelIndex <- currentLevelIndex flashcardCount startingLevel
  level <- levels !! levelIndex
  pure $ level.name

currentLevelIndex flashcardCount startingLevel = do
  levelIndex <- nextLevelIndex flashcardCount startingLevel
  if levelIndex == 0 then Nothing else Just $ levelIndex - 1

nextLevelInfo flashcardCount startingLevel = fromMaybe {wordsUntil: 0, nextLevelName: "A0"} do
  index <- nextLevelIndex flashcardCount startingLevel
  nextLevel <- levels !! index
  pure $ {wordsUntil: nextLevel.wordsNeeded - flashcardCount, nextLevelName: nextLevel.name}

nextLevelIndex flashcardCount startingLevel = findIndex moreThanCreated levels
  where moreThanCreated elem = elem.wordsNeeded > flashcardCount + startingFlashcardAmount
        startingFlashcardAmount = fromMaybe 0 do
           l <- levels !! startingLevel
           pure $ l.wordsNeeded

buildJsx props = React.do
  let redirectBook = runEffectFn2 props.navigation.navigate "Read" {}
  let redirectCreate = runEffectFn2 props.navigation.navigate "Create" {}
  let redirectReview = runEffectFn2 props.navigation.navigate "Review" {}
  let ratioDone numerator denominator = numerator <> "/" <> denominator
  languageModalVisible /\ setLanguageModalVisible <- useState false
  language /\ setLanguage <- useState (Nothing :: Maybe String)
  user <- useData (Proxy :: Proxy Query) query {fetchPolicy: "cache-and-network"}

  useEffect user.state do
     setLanguage \_ -> do
        q <- user.state
        pure $ q.currentUser.language
     pure mempty

  useEffect unit do
    launchAff_ requestPermission
    pure mempty

  useFocusEffect unit do
     user.refetch {}
     pure mempty

  case user.state of
    Nothing -> mempty
    Just u ->
      pure $ M.getJsx do
          M.childElement LanguageModal.reactComponent
            { visible: languageModalVisible
            , setVisible: setLanguageModalVisible
            , language: language
            , setLanguage: setLanguage
            }
          surface { style: M.css { flex: 1 } } do
            M.safeAreaView { style: M.css { flex: 1 } } do
              M.view {style: M.css { flex: 1, flexDirection: "row", paddingTop: 20 }} do
                let findLanguage :: {language :: String, count :: Int} -> Boolean
                    findLanguage e = e.language == fromMaybe "" language
                let languageCount = fromMaybe 0 do
                      l <- find findLanguage u.currentUser.flashcardCountPerLanguage
                      pure l.count
                let startingLevel = if Just u.currentUser.language == language then u.currentUser.startingLevel else 6
                let nextLevel = nextLevelInfo languageCount startingLevel
                topMetric "Level" $ currentLevelName languageCount startingLevel
                topMetric (i "Words Until " $ nextLevel.nextLevelName) (show nextLevel.wordsUntil)
                topMetric "Streak" $ i u.currentUser.currentStreak " days"

              M.view {style: M.css {flex: 12}} do
                listSection {} do
                  listItem {title: RN.string $ (ratioDone (show u.currentUser.dailyReadPages) (show u.currentUser.dailyGoal.pages)) <> " Pages Read", onPress: RNE.capture_ redirectBook, left: checkEmptyIcon u.currentUser.dailyReadPages u.currentUser.dailyGoal.pages, right: chevron}
                  divider {style: M.css {height: 1, width: "100%"}}
                  listItem {title: RN.string $ (ratioDone (show u.currentUser.dailyCreatedCards) (show u.currentUser.dailyGoal.created)) <> " Flashcards Created", onPress: RNE.capture_ redirectCreate, left: checkEmptyIcon u.currentUser.dailyCreatedCards u.currentUser.dailyGoal.created, right: chevron}
                  divider {style: M.css {height: 1, width: "100%"}}
                  listItem {title: RN.string $ (ratioDone (show u.currentUser.dailyReviewedSessions) (show u.currentUser.dailyGoal.reviewSessions)) <> " Sessions Completed", onPress: RNE.capture_ redirectReview, left: checkEmptyIcon u.currentUser.dailyReviewedSessions u.currentUser.dailyGoal.reviewSessions, right: chevron}
                  divider {style: M.css {height: 1, width: "100%"}}

                button {onPress: RNE.capture_ $ setLanguageModalVisible \_ -> true, style: M.css {position: "absolute", bottom: 5, right: 2}} $ M.string $ fromMaybe "Language" $ language
