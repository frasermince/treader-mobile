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

slide text ref = do
  subheading {style: textStyle} $ M.string text
  button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ next ref } $ M.string "Next"

buildJsx props = React.do
  mutationFn /\ result <- useMutation mutation {}
  ref <- useRef null
  pure $ M.getJsx $ M.view {style: surfaceStyle} do
    swiper {style: M.css {}, horizontal: true, showButtons: true, loop: false, ref: ref} do
      M.view {style: slideStyle} do
        slide "Welcome to Unchart. Here you will read in books in order to assist you in learning a language" ref
      M.view {style: slideStyle} do
        slide "While reading you will see words highlighted different colors. We use advanced machine learning to predict what part of speech each word is. Nouns will be colored orange, verbs will be colored green, and adjectives will be colored red." ref

      M.view {style: slideStyle} do
        slide "Tapping on a word will bring up translations and word information. Here you can see verb tense as well as gender" ref
      M.view {style: slideStyle} do
        slide "In the bottom box you will see longer sections of the text. Click on these at any point to get phrase and sentence translations." ref

      M.view {style: slideStyle} do
        subheading {style: textStyle} $ M.string "To progress in learning with this method you should commit to regular practice"
        button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ proceed mutationFn} $ M.string "Proceed"

mainButtonStyle = M.css
  {
    marginBottom: 15,
    width: 300,
    height: 40,
    textSize: 50
  }

slideStyle = M.css
  {
    justifyContent: "center",
    alignItems: "center",
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

textStyle = M.css
  {
    color: "black"
  }
