module FlashcardBuilder.DailySelections where
import Prelude
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Type.Proxy (Proxy(..))
import Paper (textInput, surface, button, title)
import Markup as M
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import ApolloHooks (useMutation, gql)
import Paper (textInput, surface, button, listSection, _listItem, listIcon)
import Effect.Unsafe (unsafePerformEffect)
import FlatList (flatList)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import React.Basic.Native.Events as RNE
import Debug.Trace
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import ComponentTypes (Selection)

type Props
  = { navigation :: { navigate :: EffectFn2 String { selection :: Selection } Unit } }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "DailySelectionList" $ buildJsx

type Query
  = { dailySelections :: Array Selection }

query =
  gql
    """
  query getSelections {
    dailySelections {
      word
      sentence
      phrase
      phraseOffset
      sentenceOffset
      wordLength
      book {
        id
        language
      }
    }
  }
"""


selectionMarkup redirect selection = pure $ element _listItem {title: selection.item.word, onPress: RNE.capture_ $ redirect selection.item}

buildJsx props = React.do
  result <- useData (Proxy :: Proxy Query) query {fetchPolicy: "cache-and-network"}
  case result.state of
       Nothing -> pure $ M.getJsx $ M.text {} $ M.string "Once you have read and selected words they will show up here"
       Just r -> pure $ M.getJsx do
        M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface {style: M.css { flex: 1 }} do
            flatList {data: (spy "DATA" r).dailySelections, renderItem: mkEffectFn1 $ selectionMarkup redirect, style: M.css {flex: 1}, onRefresh: result.refetch {}, refreshing: result.networkStatus == 1}
  where redirect selection = runEffectFn2 props.navigation.navigate "SentenceChoice" { selection: selection }
