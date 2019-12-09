module BottomContent where

import Prelude
import Debug.Trace (spy)
import Effect.Aff (Aff, launchAff_)
import React.Basic.Hooks (JSX, ReactComponent, component, element, useState, (/\), useRef, readRefMaybe, useEffect)
import React.Basic.Hooks as React
import React.Basic.Native as RN
import Animated (scrollView, timing, value)
import Effect.Unsafe (unsafePerformEffect)
import Record as Record
import Platform as Platform
import Effect (Effect)
import Data.Symbol (SProxy(..))
import Slider (slider)
import Effect.Uncurried (EffectFn1, mkEffectFn1)
import Data.Maybe (Maybe(..), isJust, fromMaybe)
import Markup as M
import Heterogeneous.Folding (hfoldlWithIndex)
import Paper (surface)
import Record (get)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)

valueMap = {
  "ADP": "adposition",
  "VERB": "verb",
  "NOUN": "noun",
  "PROPN": "proper noun",
  "ADJ": "adjective",
  "CONJ": "conjunction",
  "ADV": "adverb",
  "DET": "determiner",
  "Pres": "present",
  "Sing": "singular",
  "Plur": "plural",
  "Fin": "finite",
  "Masc": "masculine",
  "Fem": "feminine",
  "Inf": "infinitive",
  "AUX": "auxiliary",
  "Ger": "gerund",
  "Imp": "imperfect",
  "SCONJ": "subordinating conjunctions",
  "PRON": "pronoun"
}

mapValue :: forall sym . IsSymbol sym => SProxy sym -> String -> String
mapValue key value 
  | reflectSymbol key == "infinitive" = value
  | otherwise = reifySymbol value \x -> get x valueMap

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "BottomContent") buildJsx


type Props = { translation :: Maybe String, morphology :: Maybe {} }
styles = {
  footer: {
    --backgroundColor: "#cdcdcd",
    --paddingTop: 0,
    bottom: 0,
    height: 200, --Platform.select {ios: 64, android: 54},
    right: 0,
    left: 0,
    borderTopWidth: 1,
    borderTopColor:"#000",
    position: "absolute",
    alignItems:"center",
    justifyContent:"center",
    flexDirection: "row"
  }
}

runAnimation true fade = timing fade {toValue: 1, duration: 20}
runAnimation false fade = timing fade {toValue: 0, duration: 20}
opacity = SProxy :: SProxy "opacity"
--buildJsx :: Props -> JSX
buildJsx props = React.do
  fade /\ setFade <- useState $ value 1
  useEffect visible do
     launchAff_ $ runAnimation visible fade
     pure mempty

  pure $ M.getJsx $ surface {style: M.css $ Record.insert opacity fade styles.footer} do
     scrollView {style: M.css {height: 200, padding: 20}} do
        translationText
        maybeDataMap props.morphology
   where visible = isJust props.translation || isJust props.morphology
         translationText = fromMaybe mempty (M.text {} <$> M.string <$> props.translation)
maybeDataMap :: Maybe {} -> M.Markup Unit
maybeDataMap morphology = fromMaybe mempty (dataMap <$> morphology)
  where dataMap d = hfoldlWithIndex foldFn (mempty :: M.Markup Unit) d
        foldFn :: forall a . M.Markup Unit -> String -> SProxy a -> M.Markup Unit
        foldFn accum value key = accum <> M.view {} do
           M.view {} $ M.string "hi"--key
           M.view {} $ M.string $ mapValue key value
