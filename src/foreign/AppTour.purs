module AppTour where

import Prelude
import Effect (Effect)
import React.Basic.Hooks (unsafeRenderEffect, createContext, ReactComponent, JSX, contextProvider, UseContext, ReactContext, Pure, UseRef, provider)
import Effect.Unsafe (unsafePerformEffect)
import Data.Lazy (Lazy, defer, force)
import Data.Newtype (class Newtype)
import Effect.Uncurried (EffectFn1)
import React.Basic.Hooks (JSX, ReactComponent, componentWithChildren, element, useState, (/\), useRef, readRefMaybe, useEffect, readRef, UseEffect, UseState, Hook, coerceHook, useContext, Ref, ReactChildren, reactChildrenToArray)
import Data.Nullable (Nullable, toMaybe, toNullable, null)
import Web.DOM (Node)
import React.Basic.Hooks as React
import Data.Traversable (traverse_, traverse)
import Debug.Trace (spy)

data AppTourSequence = AppTourSequence
data TourTarget = TourTarget

foreign import tourViewFor :: forall a. Node -> Record a -> TourTarget

foreign import addTarget :: AppTourSequence -> TourTarget -> AppTourSequence

foreign import emptySequence :: AppTourSequence

foreign import showSequence :: AppTourSequence -> Effect Unit

type Context
  = {sequence :: AppTourSequence, setSequence :: (AppTourSequence -> AppTourSequence) -> Effect Unit}

providerJsx props = React.do
  sequence /\ setSequence <- useState emptySequence
  pure $ provider tourContext {sequence, setSequence} (reactChildrenToArray props.children)

tourContext :: ReactContext Context
tourContext = unsafePerformEffect $ createContext {sequence: emptySequence, setSequence: \_ -> mempty}

providerElement :: ReactComponent { children :: ReactChildren JSX}
providerElement = unsafePerformEffect $ do
  (componentWithChildren "TourProvider") providerJsx

newtype UseTourRef h
  = UseTourRef (UseEffect (Unit) (UseRef (Nullable Node) (UseContext Context h)))

derive instance ntUseTourRef :: Newtype (UseTourRef h) _

useTourRef :: forall a . Record a -> Hook UseTourRef (Ref (Nullable Node))
useTourRef options = coerceHook React.do
  {sequence, setSequence} <- useContext tourContext
  elementRef <- useRef null
  let appendToSequence element = setSequence \sequence -> addTarget sequence (tourViewFor (spy "ELEMENT TOUR" element) options)
  useEffect unit do
    result <- readRefMaybe elementRef
    traverse_ appendToSequence result
    pure mempty
  pure elementRef
