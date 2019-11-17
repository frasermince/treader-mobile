module SignIn where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as React
import Markup as M
import Paper (textInput, surface, button)
import React.Basic.Events (EventFn, unsafeEventFn)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import Unsafe.Coerce (unsafeCoerce)
import ApolloHooks (useMutation, gql, QueryState(..), DocumentNode)
import Debug.Trace
import AsyncStorage (setItem)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_)
import Effect.Uncurried (runEffectFn1, EffectFn1)

type Props
  = {navigation :: {navigate :: EffectFn1 String Unit}}

mutation :: DocumentNode
mutation = gql
  """
mutation loginMutation($input: LoginInput!) {
  login(input: $input) {
    session {token}
  }
}
  """

text :: EventFn (RNE.NativeSyntheticEvent String) String
text = unsafeEventFn \e -> (unsafeCoerce e)

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "SignIn") buildJsx

changeField setField = RNE.handler text \t ->
  setField \_ -> t

buildJsx props = React.do
  mutate /\ d <- useMutation mutation {}
  email /\ setEmail <- useState ""
  password /\ setPassword <- useState ""
  pure $ M.getJsx do
     surface {} do
        textInput {label: "Email", onChangeText: changeField setEmail, value: email}
        textInput {label: "Password", onChangeText: changeField setPassword, value: password}
        button {onPress: RNE.capture_ (press mutate email password)} (M.jsx $ RN.string "submit")
  where press mutate email password = launchAff_ do
          result <- mutate $ {variables: {input: {email, password}}}
          let session = "Bearer " <> result.data.login.session.token
          spy "here" $ setItem "treader-session" session
          liftEffect $ runEffectFn1 props.navigation.navigate "App"
