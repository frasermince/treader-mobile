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
import Paper (surface)
import Record (get)
import Data.Symbol (class IsSymbol, reflectSymbol, reifySymbol)
import Foreign.Object (lookup, Object, fold)
import Morphology (valueNames)

mapValue ::  String -> String -> String
mapValue "infinitive" value = value
mapValue key value = fromMaybe (spy "value" value) $ spy "lookup" $ lookup value $ spy "names" valueNames

reactComponent :: ReactComponent Props
reactComponent =
  unsafePerformEffect
    $ do
        (component "BottomContent") buildJsx


type Props = { translation :: Maybe String, morphology :: Maybe (Object String) }
styles fade = {
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
  flexDirection: "row",
  opacity: fade,
  zIndex: zIndex
}
  where
    zIndex :: Int
    zIndex = fade.interpolate {
      inputRange: [ 0, 1],
      outputRange: [-1, 9]
    }

runAnimation true fade = timing fade {toValue: 1, duration: 20}
runAnimation false fade = timing fade {toValue: 0, duration: 20}
--buildJsx :: Props -> JSX
buildJsx props = React.do
  fade /\ setFade <- useState $ value 1
  useEffect visible do
     launchAff_ $ runAnimation visible fade
     pure mempty

  pure $ M.getJsx $ surface {style: M.css $ styles fade} do
     scrollView {style: M.css {height: 200, padding: 20}} do
        translationText
        maybeDataMap props.morphology
   where visible = isJust props.translation || isJust props.morphology
         translationText = fromMaybe mempty (M.text {} <$> M.string <$> (append "Translation: ") <$> props.translation)
maybeDataMap :: Maybe (Object String) -> M.Markup Unit
maybeDataMap morphology = fromMaybe mempty (dataMap <$> spy "MORPHOLOGY" morphology)
  where dataMap d = fold foldFn (mempty :: M.Markup Unit) d
        foldFn :: forall a . M.Markup Unit -> String -> String -> M.Markup Unit
        foldFn accum key value = accum <> M.view {style: M.css {flex: 1, alignSelf: "stretch", flexDirection: "row"}} do
           M.view {style: M.css {flex: 1, alignSelf: "stretch"}} $ M.text {} $ M.string key
           M.view {style: M.css {flex: 1, alignSelf: "stretch"}} $ M.text {} $ M.string $ mapValue key value
