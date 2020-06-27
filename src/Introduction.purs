module Introduction where

import Prelude
import Paper (surface, title, divider, button, modal, subheading, headline, listItem, listIcon)
import Markup as M
import Swiper (swiper)
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import React.Basic.Native.Events as RNE
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext)
import Data.Traversable (traverse_)
import Effect.Unsafe (unsafePerformEffect)
import Effect (Effect)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1, mkEffectFn2)
import ApolloHooks (useMutation, gql, DocumentNode)
import Record.Unsafe.Union (unsafeUnion)
import Effect.Aff (Aff, launchAff_, try)
import Data.Interpolate (i)
import Data.Maybe (Maybe(..), isNothing, fromMaybe)
import Data.Array ((!!))
import FirebaseMessaging (requestPermission)
import CefrLevels (levels)
import TranslateLanguages (languages)

type Props = {}

mutation :: DocumentNode
mutation =
  gql
    """
mutation createGoals($input: CreateGoalsInput!) {
  createGoals(input: $input) {
    user {
      id
      iosVersion
      nativeLanguage
      startingLevel
    }
  }
}
  """

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Introduction" $ buildJsx

radio Nothing choice p = element listIcon $ unsafeUnion p { color: "#000", icon: "radiobox-blank" }
radio (Just selected) choice p
  | selected == choice = element listIcon $ unsafeUnion p { color: "#000", icon: "radiobox-marked" }
  | otherwise = element listIcon $ unsafeUnion p { color: "#000", icon: "radiobox-blank" }

neededWordsIcon number p = M.getJsx $ M.text {style: M.css {marginTop: 12}} $ M.string $ "~" <> show number <> " words"

daysToGoal created (Just level) (Just levelCurrent) p = M.getJsx $ M.text {style: M.css {marginTop: 12}} $ M.string $ (show $ (level.wordsNeeded - levelCurrent.wordsNeeded) / created) <> " days\n Until " <> level.name
daysToGoal _ _ _ p = mempty

previous ref = do
  result <- readRefMaybe ref
  traverse_ (\s -> runEffectFn1 s.scrollBy (-1)) result

next ref = do
  result <- readRefMaybe ref
  traverse_ (\s -> runEffectFn1 s.scrollBy 1) result

proceed mutationFn (Just dailyGoalId) (Just levelGoal) (Just language) (Just currentLevel) (Just nativeLanguageSelection) = launchAff_ do
  requestPermission
  mutationFn {variables: {input: {iosVersion: "1.4.6", dailyGoalId, levelGoal, language: language, startingLevel: currentLevel, nativeLanguage: nativeLanguageSelection}}}
proceed mutationFn _ _ _ _ _ = mempty

slide buttonText onPress textComponent = do
  textComponent
  M.view {style: M.css {flex: 1, alignItems: "center"}} do
      button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ onPress } $ M.string buttonText

initialSlide heading text ref = slide "Next" (next ref) do
    M.view {style: M.css {flex: 8}} do
      M.view {style: textContainerStyle} do
          title {} $ M.string heading
          subheading {style: textStyle} $ M.string text

goalChoice :: Int -> Maybe Int -> ((Maybe Int -> Maybe Int) -> Effect Unit) -> M.Markup Unit
goalChoice choice selection setSelection = do
  M.view {style: M.css {alignItems: "center", width: "100%"}} do
    divider {style: M.css {height: 1, width: "100%", color: "#66aab1"}}
    listItem {
      style: M.css {color: "black", width: "100%"},
      descriptionNumberOfLines: 4,
      title: level.name,
      description: selectedDescription,
      onPress: RNE.capture_ $ setSelection \_ -> Just choice,
      left: radio selection choice,
      right: neededWordsIcon $ level.wordsNeeded
    }
  where level = fromMaybe {name: "", description: "", wordsNeeded: 0} $ levels !! choice
        isSelected = fromMaybe false do
          s <- selection
          pure $ choice == s
        selectedDescription
          | isSelected = level.description
          | otherwise = ""

dailyCommitmentChoice :: Int -> Int -> Int -> Int -> Int -> Maybe Int -> Maybe Int -> ((Maybe Int -> Maybe Int) -> Effect Unit) -> Maybe Int -> M.Markup Unit
dailyCommitmentChoice minutes pages created sessions choice selection goalSelected setSelection currentLevel = do
  M.view {style: M.css {alignItems: "center", width: "100%"}} do
    divider {style: M.css {height: 1, width: "100%", color: "#66aab1"}}
    listItem {
      style: M.css {color: "black", width: "100%"},
      descriptionNumberOfLines: 3,
      title: title,
      description: description,
      onPress: RNE.capture_ $ setSelection \_ -> Just choice,
      left: radio selection choice,
      right: daysToGoal created levelSelected wordsForCurrent
    }
    where description :: String
          description = i "Read " pages " pages a day \nCreate " created " flashcards\nComplete " sessions " review sessions"
          title = M.getJsx $ M.text {style: M.css {fontWeight: "bold"}} $ M.string $ i "~" minutes " minutes a day"
          wordsForCurrent = do
             g <- currentLevel
             levels !! g
          levelSelected = do
             g <- goalSelected
             levels !! g

flatListLanguageChoice value setSelection i = pure $ M.getJsx $ languageChoice value i.item.code i.item.name setSelection
languageChoice :: Maybe String -> String -> String -> ((Maybe String -> Maybe String) -> Effect Unit) -> M.Markup Unit
languageChoice value choice label setSelection = do
  M.view {style: M.css {alignItems: "center", width: "100%"}} do
    divider {style: M.css {height: 1, width: "100%", color: "#66aab1"}}
    listItem {
      style: M.css {color: "black", width: "100%"},
      title: label,
      onPress: RNE.capture_ $ setSelection \_ -> Just choice,
      left: radio value choice
    }

buildJsx props = React.do
  mutationFn /\ result <- useMutation mutation {}
  dailySelection /\ setDailySelection <- useState (Nothing :: Maybe Int)
  goalSelection /\ setGoalSelection <- useState (Nothing :: Maybe Int)
  currentLevel /\ setCurrentLevel <- useState (Nothing :: Maybe Int)
  languageSelection /\ setLanguageSelection <- useState (Nothing :: Maybe String)
  nativeLanguageSelection /\ setNativeLanguageSelection <- useState (Nothing :: Maybe String)
  ref <- useRef null
  pure $ M.getJsx $ M.view {style: surfaceStyle} do
    swiper {style: M.css {height: "100%"}, horizontal: true, showButtons: true, loop: false, ref: ref} do
      M.view {style: slideStyle} do
         initialSlide "Welcome to Unchart" "Unchart allows you to fully utilize one of your best resources for language learning, novels. This is done using a three step process" ref

      M.view {style: slideStyle} do
         initialSlide "Read Books" "This is a highly effective way to gain grammar and vocabulary and we make it as simple as possible with powerful tools to quickly get part of speech information and translations." ref

      M.view {style: slideStyle} do
         initialSlide "Create Flashcards" "You use the words you just read to create visual flashcards. Creating the flashcards yourself makes them memorable and pairing the words with images allows you to avoid translating." ref

      M.view {style: slideStyle} do
         initialSlide "Review Flashcards" "We use a spaced repetition system so you avoid reviewing a flashcard until it is needed! This increases the amount of words you can memorize." ref

      M.view {style: slideStyle} do
          M.view {style: M.css {flex: 8}} do
            M.view {style: M.css {alignItems: "center", height: "100%", marginTop: 45}} do
              M.view {style: M.css {flex: 2, alignItems: "center", height: "100%"}} do
                title {style: M.css {marginBottom: 20}} $ M.string "Choose Your Native Language"
                subheading {style: textStyle} $ M.string "This will be the language we provide translations to."
              M.view {style: M.css {flex: 6, alignItems: "center", height: "100%", width: "100%"}} do
                M.flatList {
                  data: languages,
                  renderItem: mkEffectFn1 $ flatListLanguageChoice nativeLanguageSelection setNativeLanguageSelection,
                  style: M.css {flex: 1, width: "100%"},
                  keyExtractor: mkEffectFn2 \i n -> pure i.code
                }
              M.view {style: M.css {flex: 1, alignItems: "center", marginBottom: 50, marginTop: 30}} do
                button { disabled: isNothing nativeLanguageSelection, mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ next ref } $ M.string "Next"


      M.view {style: slideStyle} do
          M.view {style: M.css {flex: 8}} do
            M.view {style: M.css {alignItems: "center", height: "100%", marginTop: 45}} do
              M.view {style: M.css {flex: 2, alignItems: "center", height: "100%"}} do
                title {style: M.css {marginBottom: 20}} $ M.string "Choose Your Target Language"
                subheading {style: textStyle} $ M.string "Choose the language you want to learn."
              M.view {style: M.css {flex: 6, alignItems: "center", height: "100%", width: "100%"}} do
                languageChoice languageSelection "fr" "French" setLanguageSelection
                languageChoice languageSelection "es" "Spanish" setLanguageSelection
                languageChoice languageSelection "it" "Italian" setLanguageSelection
                languageChoice languageSelection "de" "German" setLanguageSelection
                languageChoice languageSelection "ru" "Russian" setLanguageSelection
                languageChoice languageSelection "en" "English" setLanguageSelection
                divider {style: M.css {height: 1, width: "100%", color: "#66aab1"}}
          M.view {style: M.css {flex: 1, alignItems: "center"}} do
            button { disabled: isNothing languageSelection, mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ next ref } $ M.string "Next"

      M.view {style: slideStyle} do
          M.view {style: M.css {flex: 8}} do
            M.view {style: M.css {alignItems: "center", height: "100%", marginTop: 45}} do
              M.view {style: M.css {flex: 2, alignItems: "center", height: "100%"}} do
                title {style: M.css {marginBottom: 20}} $ M.string "Choose Your Current Level"
                subheading {style: textStyle} $ M.string "This will help us keep track of your progress"
              M.view {style: M.css {flex: 6, alignItems: "center", height: "100%", width: "100%"}} do
                  goalChoice 6 currentLevel setCurrentLevel
                  goalChoice 0 currentLevel setCurrentLevel
                  goalChoice 1 currentLevel setCurrentLevel
                  goalChoice 2 currentLevel setCurrentLevel
                  goalChoice 3 currentLevel setCurrentLevel
                  goalChoice 4 currentLevel setCurrentLevel
                  goalChoice 5 currentLevel setCurrentLevel
                  divider {style: M.css {height: 1, width: "100%", color: "#66aab1"}}
          M.view {style: M.css {flex: 1, alignItems: "center"}} do
            button { disabled: isNothing currentLevel, mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ next ref } $ M.string "Next"


      M.view {style: slideStyle} do
          M.view {style: M.css {flex: 8}} do
            M.view {style: M.css {alignItems: "center", height: "100%", marginTop: 45}} do
              M.view {style: M.css {flex: 2, alignItems: "center", height: "100%"}} do
                title {style: M.css {marginBottom: 20}} $ M.string "Choose Your Long Term Goal"
                subheading {style: textStyle} $ M.string "Language learning can be broken down into levels of fluency. It's important to choose a managable but exciting goal to work towards."
              M.view {style: M.css {flex: 6, alignItems: "center", height: "100%", width: "100%"}} do
                  goalChoice 0 goalSelection setGoalSelection
                  goalChoice 1 goalSelection setGoalSelection
                  goalChoice 2 goalSelection setGoalSelection
                  goalChoice 3 goalSelection setGoalSelection
                  goalChoice 4 goalSelection setGoalSelection
                  goalChoice 5 goalSelection setGoalSelection
                  divider {style: M.css {height: 1, width: "100%", color: "#66aab1"}}
          M.view {style: M.css {flex: 1, alignItems: "center"}} do
            button { disabled: isNothing goalSelection, mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ next ref } $ M.string "Next"

      M.view {style: slideStyle} do
          M.view {style: M.css {flex: 5}} do
            M.view {style: M.css {alignItems: "center", height: "100%", marginTop: 45}} do
              M.view {style: M.css {flex: 2, alignItems: "center", height: "100%"}} do
                title {style: M.css {marginBottom: 20}} $ M.string "Choose your daily commitment"
                subheading {style: textStyle} $ M.string "Progressing in language learning requires regular practice. Choose your daily goal to get started!"
              M.view {style: M.css {flex: 6, alignItems: "center", height: "100%", width: "100%"}} do
                  dailyCommitmentChoice 30 4 10 1 1 dailySelection goalSelection setDailySelection currentLevel
                  dailyCommitmentChoice 45 6 20 2 2 dailySelection goalSelection setDailySelection currentLevel
                  dailyCommitmentChoice 60 8 30 3 3 dailySelection goalSelection setDailySelection currentLevel
                  divider {style: M.css {height: 1, width: "100%", color: "#66aab1"}}
          M.view {style: M.css {flex: 1, alignItems: "center", flexDirection: "row", alignContent: "space-between"}} do
            button { style: endButtonStyle, mode: "outlined", onPress: RNE.capture_ $ previous ref } $ M.string "Back"
            button { disabled: isNothing dailySelection || isNothing goalSelection || isNothing currentLevel || isNothing nativeLanguageSelection, mode: "contained", style: endButtonStyle, onPress: RNE.capture_ $ proceed mutationFn dailySelection goalSelection languageSelection currentLevel nativeLanguageSelection} $ M.string "Get started"


mainButtonStyle = M.css
  {
    width: 300,
    marginBottom: 15,
    height: 40,
    justifyContent: "flex-end",
    textSize: 50
  }

endButtonStyle = M.css
  {
    flex: 1,
    marginBottom: 15,
    height: 40,
    justifyContent: "flex-end",
    textSize: 50
  }

slideStyle = M.css
  {
    marginLeft: 20,
    marginRight: 20,
    height: "100%"
  }
surfaceStyle = M.css
  {
    borderRadius: 10,
    backgroundColor: "white",
    justifyContent: "center",
    alignItems: "center",
    width: "100%",
    flex: 1
  }

textContainerStyle = M.css
  {
    height: "100%",
    justifyContent: "center",
    alignItems: "center"
  }

textStyle = M.css
  {
    color: "black"
  }
