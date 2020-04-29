module FlashcardReview.Index where
import Prelude
import React.Basic.Hooks as React
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext)
import QueryHooks (useData, UseData, stripGraphqlError)
import ApolloHooks (useMutation, gql)
import Type.Proxy (Proxy(..))
import Effect.Unsafe (unsafePerformEffect)
import Data.Maybe (Maybe(..), fromMaybe, isNothing, isJust)
import Markup as M
import Paper (textInput, surface, button, title, divider, listItem, paragraph, headline, badge, iconButton, fab, dialog, dialogContent, dialogActions, dialogTitle, portal, searchbar, listIcon)
import React.Basic.Native.Events as RNE
import Data.Array (length)
import Effect.Uncurried (runEffectFn1, EffectFn1, runEffectFn2, EffectFn2)
import Navigation (useFocusEffect)

mainButtonStyle = M.css
  {
    marginBottom: 15,
    width: 300,
    height: 40,
    textSize: 50
  }

type Props
  = { navigation :: { navigate :: EffectFn2 String {} Unit } }

type Query = {flashcards :: Array {id :: String}}

query =
  gql
    """
    query getFlashcards {
      flashcards {
        id
      }
    }
  """

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        component "ReviewIndex" $ buildJsx


buildJsx props = React.do
  flashcardsResult <- useData (Proxy :: Proxy Query) query { errorPolicy: "all", fetchPolicy: "cache-and-network" }
  useFocusEffect unit do
     flashcardsResult.refetch {}
     pure mempty

  case flashcardsResult.state of
       Nothing -> mempty
       Just d ->
         pure $ M.getJsx do
           M.safeAreaView { style: M.css { flex: 1, backgroundColor: "#ffffff" } } do
            title {style: M.css {marginTop: "10%", textAlign: "center", flex: 2}} $ M.string $ "Start a review of " <> (show $ min 30 (length d.flashcards)) <> " flashcards"
            M.view {style: M.css {flex: 3, alignItems: "center"}} do
              button { mode: "contained", style: mainButtonStyle, onPress: RNE.capture_ $ runEffectFn2 props.navigation.navigate "Review" {}} $ M.string $ "Start Review"
