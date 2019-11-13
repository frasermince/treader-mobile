module SignIn where

import Prelude
import React.Basic.Native as RN
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\))
import React.Basic.Hooks as React
import Markup as M
import Paper (textInput)

type Props = {}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "SignIn") buildJsx

buildJsx props = React.do
  email /\ setEmail <- useState ""
  password /\ setPassword <- useState ""
  pure $ M.getJsx do
     M.childElement textInput {label: "Email", onChangeText: setEmail, value: email}
     M.childElement textInput {label: "Password", onChangeText: setPassword, value: password}
