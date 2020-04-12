module FlashcardBuilder.WordSelection where

import Prelude
import Markup as M
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab)
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Effect.Unsafe (unsafePerformEffect)

type Props = {}

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "WordSelection" $ buildJsx

buildJsx props = React.do
  pure $ M.getJsx $ M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
    surface { style: M.css { flex: 1 } } do
       headline {} $ M.string "Select More Words"
