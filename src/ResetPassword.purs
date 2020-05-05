module ResetPassword where

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
import Effect.Uncurried (runEffectFn1, EffectFn1)
import Data.Traversable (traverse_)
import Effect.Exception (message)
import Data.String (stripPrefix, Pattern(..))
import Data.Maybe (fromMaybe)
import Keyboard (dismiss)
import Linking (openUrl)

type Props
  = {}

mutation :: DocumentNode
mutation =
  gql
    """
mutation resetMutation($input: SendResetEmailInput!) {
  sendResetEmail(input: $input) {
    result
  }
}
  """

text :: EventFn (RNE.NativeSyntheticEvent String) String
text = unsafeEventFn \e -> (unsafeCoerce e)

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "ResetPassword") buildJsx

changeField setField =
  RNE.handler text \t ->
    setField \_ -> t

buildJsx props = React.do
  { setLoading, setError } <- useContext dataStateContext
  mutate /\ d <- useMutation mutation { errorPolicy: "all" }
  email /\ setEmail <- useState ""
  pure
    $ M.getJsx do
        surface {style: M.css {flex: 1, paddingLeft: 10, paddingRight: 10}} do
          M.view {style: M.css {marginTop: 10, marginBottom: 10}} do
            textInput { label: "Email", onChangeText: changeField setEmail, value: email, autoCapitalize: "none" }
            button { mode: "contained", onPress: RNE.capture_ (press mutate email setError setLoading) } $ M.string "Reset Password"
    where
    stripGraphqlError message = fromMaybe message $ stripPrefix (Pattern "GraphQL error: ") message

    press mutate email setError setLoading =
      launchAff_ do
        liftEffect $ dismiss
        result <- try $ mutate $ { variables: { input: { email } } }
        case result of
          Left error -> liftEffect $ runEffectFn1 setError $ stripGraphqlError $ message error
          Right resp -> mempty
