module FlashcardBuilder.DailySelections where
import Prelude
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Record.Unsafe.Union (unsafeUnion)
import Type.Proxy (Proxy(..))
import Paper (textInput, surface, button, title, caption, divider)
import Markup as M
import Effect.Uncurried (runEffectFn1, EffectFn1, mkEffectFn1)
import Data.Maybe (Maybe(..), fromMaybe, isNothing)
import ApolloHooks (useMutation, gql)
import Paper (textInput, surface, button, listSection, listItem, listIcon, title)
import Effect.Unsafe (unsafePerformEffect)
import FlatList (flatList)
import Effect.Uncurried (runEffectFn2, EffectFn2)
import React.Basic.Native.Events as RNE
import Debug.Trace
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook)
import ComponentTypes (Selection)
import Navigation (useFocusEffect)

type Props
  = { navigation :: { navigate :: EffectFn2 String { selection :: Selection } Unit } }

chevron p = element listIcon $ unsafeUnion p { color: "#000", icon: "chevron-right" }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "DailySelection" $ buildJsx

type Query
  = { dailySelections :: Array Selection }

query =
  gql
    """
  query getSelections {
    dailySelections {
      id
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


selectionMarkup redirect selection = pure $ M.getJsx $ do
  listItem {title: M.getJsx $ M.text {style: M.css {fontWeight: "bold"}} $ M.string selection.item.word, description: selection.item.sentence, onPress: RNE.capture_ $ redirect selection.item, right: chevron}
  divider {style: M.css {height: 1, width: "100%", color: "#66aab1"}}

emptyView = M.getJsx $ M.view {style: M.css {flex: 1, justifyContent: "center", alignItems: "center"}} do
  M.text {} $ M.string "Upon selecting words use this page to create flashcards"

buildJsx props = React.do
  result <- useData (Proxy :: Proxy Query) query {fetchPolicy: "cache-and-network"}
  useFocusEffect unit do
     result.refetch {}
     pure mempty
  case result.state of
       Nothing -> pure $ M.getJsx $ M.text {} $ M.string "Once you have read and selected sentences they will show up here"
       Just r -> pure $ M.getJsx do
        M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface {style: M.css { flex: 1 }} do
            M.view {style: M.css {alignItems: "center"}} do
              caption {style: M.css {}} $ M.string "Words selected while reading will appear here"
            flatList {data: (spy "DATA" r).dailySelections, renderItem: mkEffectFn1 $ selectionMarkup redirect, style: M.css {flex: 1}, onRefresh: result.refetch {}, refreshing: result.networkStatus == 1, "ListEmptyComponent": emptyView}
  where redirect selection = runEffectFn2 props.navigation.navigate "SentenceChoice" { selection: selection }
