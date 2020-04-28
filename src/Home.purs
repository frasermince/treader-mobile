module Home where
import Prelude
import Effect.Unsafe (unsafePerformEffect)
import React.Basic.Native as RN
import React.Basic.Native.Events as RNE
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, useEffect, (/\))
import Markup as M
import Paper (textInput, surface, button, listSection, listItem, listIcon, divider, title)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import Record.Unsafe.Union (unsafeUnion)

type Props = { navigation :: { navigate :: EffectFn2 String {} Unit } }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "Home") buildJsx

buildJsx props = React.do
  let redirectBook = runEffectFn2 props.navigation.navigate "Read" {}
  let redirectCreate = runEffectFn2 props.navigation.navigate "Create" {}
  let redirectReview = runEffectFn2 props.navigation.navigate "Review" {}
  let ratioDone p = M.getJsx $ M.text {style: M.css {marginTop: 8}} $ M.string "0/10"
  let checkEmptyIcon p = element listIcon $ unsafeUnion p { color: "#000", icon: "checkbox-blank-outline" }

  pure $ M.getJsx
    $ M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
       surface { style: M.css { flex: 1 } } do
          title { style: M.css {textAlign: "center"}} $ M.string "Daily Tasks"
          listSection {} do
             listItem {title: RN.string "Read 10 Pages", onPress: RNE.capture_ redirectBook, left: checkEmptyIcon, right: ratioDone}
             divider {style: M.css {height: 1, width: "100%"}}
             listItem {title: RN.string "Create 10 Flashcards", onPress: RNE.capture_ redirectCreate, left: checkEmptyIcon, right: ratioDone }
             divider {style: M.css {height: 1, width: "100%"}}
             listItem {title: RN.string "Review 10 Flashcards", onPress: RNE.capture_ redirectReview, left: checkEmptyIcon, right: ratioDone}
             divider {style: M.css {height: 1, width: "100%"}}

