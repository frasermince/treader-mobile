module FlashcardBuilder.WordGrid where
import Prelude
import Effect (Effect)
import Markup as M
import React.Basic.Hooks (JSX, ReactComponent, component, useState, (/\), useEffect, useContext, element)
import Effect.Unsafe (unsafePerformEffect)
import Effect.Uncurried (EffectFn1, mkEffectFn1, runEffectFn1, mkEffectFn2, runEffectFn2, EffectFn2)
import ComponentTypes (FlashcardOffsetTranslation)
import React.Basic.Native.Events (NativeSyntheticEvent, handler, nativeEvent, timeStamp, capture_) as RNE

type Props = {header :: JSX, words :: Array FlashcardOffsetTranslation, redirect :: String -> String -> Int -> Effect Unit}
reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "WordGrid" $ buildJsx

wordElement redirect w = pure $ M.getJsx do
  M.touchableOpacity {style: M.css {padding: 20, margin: "1.5%", borderWidth: 1, flex: 1, borderColor: "gray"}, onPress: RNE.capture_ $ redirect w.item.word w.item.translation w.item.offset} do
    M.text {} $ M.string w.item.word

buildJsx props = React.do
  pure $ M.getJsx $ M.flatList {
    data: props.words,
    renderItem: mkEffectFn1 $ wordElement props.redirect,
    style: M.css {minWidth: "95%"},
    "ListHeaderComponent": props.header,
    contentContainerStyle: M.css {minWidth: "95%", margin: 10}, numColumns: 2.0
  }
