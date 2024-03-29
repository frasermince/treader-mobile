module FlashcardBuilder.DailySelections where
import Prelude
import React.Basic.Hooks as React
import QueryHooks (useData, UseData)
import Record.Unsafe.Union (unsafeUnion)
import Icon (materialIcon)
import Type.Proxy (Proxy(..))
import Paper (textInput, surface, button, title, caption, divider, headline, subheading)
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
import Navigation (useFocusEffect)
import Icon (materialIcon)

type Props
  = { navigation :: { navigate :: EffectFn2 String { id :: String } Unit } }

type DailySelection = {id :: Int, word :: String, sentence :: String, phrase :: String, sentenceOffset :: Int, phraseOffset :: Int, wordLength :: Int, book :: {language :: String, id :: Int}}

chevron p = element listIcon $ unsafeUnion p { color: "#000", icon: "chevron-right" }

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "DailySelection" $ buildJsx

type Query
  = { dailySelections :: Array DailySelection }

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
       Nothing -> mempty
       (Just {dailySelections: []}) -> pure $ M.getJsx do
          surface {style: M.css { flex: 1 }} do
            M.view {style: M.css {alignItems: "center", height: "100%", marginTop: "55%", marginLeft: "10%", marginRight: "10%", textAlign: "center"}} do
              headline {style: M.css {marginBottom: 20, fontSize: 28}} $ M.string "Well this is empty..."
              subheading {style: M.css {textAlign: "center", lineHeight: 36, fontSize: 20, flexDirection: "row", flexWrap: "wrap", justifyContent: "space-between", alignItems: "center", flexShrink: 1}} $ do
                 M.text {style: M.css {}} $ M.string "Create flashcards from what you read. Tap on the  "
                 materialIcon { name: "book", size: 32, style: M.css { marginTop: 10} }
                 M.text {style: M.css {}} $ M.string "  icon and as you read words will show up here"
       Just r -> pure $ M.getJsx do
        M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
          surface {style: M.css { flex: 1 }} do
            M.view {style: M.css {alignItems: "center", marginTop: 10}} do
              caption {style: M.css {}} $ M.string "Words selected while reading will appear here"
            flatList {data: (spy "DATA" r).dailySelections, renderItem: mkEffectFn1 $ selectionMarkup redirect, style: M.css {flex: 1}, onRefresh: result.refetch {}, refreshing: result.networkStatus == 1}
  where redirect selection = runEffectFn2 props.navigation.navigate "SentenceChoice" { id: selection.id }
