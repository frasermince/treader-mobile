module Introduction where

import Prelude
import Paper (surface, title, divider, button, modal, subheading, headline)
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
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)
import ApolloHooks (useMutation, gql, DocumentNode)
import Effect.Aff (Aff, launchAff_, try)

type Props = {}

mutation :: DocumentNode
mutation =
  gql
    """
mutation updateCurrentUser($input: UserInput!) {
  update_user(input: $input) {
    user {
      id
      isSubscribed
      isPermitted
      showPayment
      iosVersion
    }
  }
}
  """

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "Introduction" $ buildJsx

next ref = do
  result <- readRefMaybe ref
  traverse_ (\s -> runEffectFn1 s.scrollBy 1) result

proceed mutationFn = launchAff_ $ mutationFn {variables: {input: {iosVersion: "1.3.4"}}}

slide heading text buttonText onPress = do
  M.view {style: M.css {flex: 8}} do
    M.view {style: textContainerStyle} do
      title {} $ M.string heading
      subheading {style: textStyle} $ M.string text
  M.view {style: M.css {flex: 1, alignItems: "center"}} do
     button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ onPress } $ M.string buttonText

initialSlide heading text ref = slide heading text "Next" (next ref)

buildJsx props = React.do
  mutationFn /\ result <- useMutation mutation {}
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
         slide "Commit" "Committing to learning daily can get you to your goals in no time" "Proceed" $ proceed mutationFn

mainButtonStyle = M.css
  {
    marginBottom: 15,
    width: 300,
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
