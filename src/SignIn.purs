module SignIn where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useContext)
import React.Basic.Hooks as React
import Markup as M
import Paper (textInput, surface, button, title)
import React.Basic.Events (EventFn, unsafeEventFn)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE
import Unsafe.Coerce (unsafeCoerce)
import ApolloHooks (useMutation, gql, QueryState(..), DocumentNode, useApolloClient)
import Debug.Trace
import AsyncStorage (setItem)
import Context (dataStateContext, Context)
import Effect.Class (liftEffect)
import Effect.Aff (Aff, launchAff_, try)
import Data.Either (Either(..))
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Data.Traversable (traverse_)
import Effect.Exception (message)
import Data.String (stripPrefix, Pattern(..))
import Data.Maybe (fromMaybe)
import Keyboard (dismiss)
import Linking (openUrl)

type Props
  = { navigation :: { navigate :: EffectFn2 String {} Unit } }

mutation :: DocumentNode
mutation =
  gql
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

changeField setField =
  RNE.handler text \t ->
    setField \_ -> t

buildJsx props = React.do
  client <- useApolloClient
  { setLoading, setError } <- useContext dataStateContext
  mutate /\ d <- useMutation mutation { errorPolicy: "all" }
  email /\ setEmail <- useState ""
  password /\ setPassword <- useState ""
  pure
    $ M.getJsx do
        surface {style: M.css {flex: 1, paddingLeft: 10, paddingRight: 10}} do
          M.view {style: M.css {marginTop: 10, marginBottom: 10}} do
            textInput { label: "Email", onChangeText: changeField setEmail, value: email, autoCapitalize: "none" }
            textInput { label: "Password", onChangeText: changeField setPassword, value: password, secureTextEntry: true, autoCapitalize: "none" }
            button { mode: "contained", onPress: RNE.capture_ (press mutate email password client setError) } $ M.string "Login"
          M.text {style: M.css {textAlign: "center", marginTop: 10}, onPress: RNE.capture_ $ runEffectFn2 props.navigation.navigate "ResetPassword" {}} $ M.string "Forgot Password"
    where
    stripGraphqlError message = fromMaybe message $ stripPrefix (Pattern "GraphQL error: ") message

    press mutate email password client setError =
      launchAff_ do
        result <- try $ mutate $ { variables: { input: { email, password } } }
        case result of
          Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
          Right resp -> do
            let
              session = "Bearer " <> resp.login.session.token
            liftEffect $ traverse_ _.resetStore client
            setItem "treader-session" session
        --    liftEffect $ runEffectFn1 props.navigation.navigate "App"
        liftEffect $ dismiss
