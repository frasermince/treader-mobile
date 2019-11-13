module SignIn where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as React
import Markup as M
import Paper (textInput, surface, button)
import React.Basic.Events (EventFn, unsafeEventFn)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp) as RNE
import Unsafe.Coerce (unsafeCoerce)
import Debug.Trace

type Props = {}

text :: EventFn (RNE.NativeSyntheticEvent String) String
text = unsafeEventFn \e -> (unsafeCoerce e).text

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "SignIn") buildJsx

changeField setField = RNE.handler text \text ->
  setField \_ -> (spy "event" text)

buildJsx props = React.do
  email /\ setEmail <- useState ""
  password /\ setPassword <- useState ""
  pure $ M.getJsx do
     surface {} do
        textInput {label: "Email", onChangeText: changeField setEmail, value: email}
        textInput {label: "Password", onChangeText: changeField setPassword, value: password}
        button {} (M.jsx $ RN.string "submit")
