module Markup where

import Prelude
import React.Basic.DOM.Internal (CSS)
import React.Basic.Native as RN
import React.Basic.Hooks (JSX)
import Unsafe.Coerce (unsafeCoerce)
import Prim.Row (class Lacks)
import Data.Tuple
import Record as Record
import Data.Symbol (SProxy(..))
import React.Basic.Hooks as H
import Debug.Trace (spy)
import Data.Maybe (fromMaybe)
import Data.Array (head)
import Data.Foldable (foldl)

newtype Markup a
  = Markup (Tuple (Array JSX) a)

instance semigroupMarkup :: Semigroup a => Semigroup (Markup a) where
  append a b = Markup (Tuple jsx value)
    where
    jsx = (getChildren a) <> (getChildren b)

    value = (runMarkup a) <> (runMarkup b)

instance monoidMarkup :: Monoid a => Monoid (Markup a) where
  mempty = Markup (Tuple mempty mempty)

instance functorMarkup :: Functor (Markup) where
  map f (Markup (Tuple jsx a)) = Markup (Tuple jsx (f a))

instance applyMarkup :: Apply (Markup) where
  apply (Markup (Tuple firstJsx fn)) (Markup (Tuple secondJsx a)) = Markup (Tuple (firstJsx <> secondJsx) (fn a))

instance applicativeMarkup :: Applicative Markup where
  pure x = Markup (Tuple mempty x)

instance bindMarkup :: Bind (Markup) where
  bind (Markup (Tuple jsx a)) f = (Markup (Tuple (jsx <> newJsx) newA))
    where
    (Markup (Tuple newJsx newA)) = f a

instance monadMarkup :: Monad Markup

getChildren :: forall a. Markup a -> Array JSX
getChildren (Markup (Tuple jsx a)) = jsx

getJsx :: forall a. Markup a -> JSX
getJsx (Markup (Tuple jsx a)) = (foldl (\accum m -> accum <> m) mempty jsx)

runMarkup :: forall a. Markup a -> a
runMarkup (Markup (Tuple jsx a)) = a

jsx :: Array JSX -> Markup Unit
jsx j = Markup (Tuple (j) unit)

childrenProxy = SProxy :: SProxy "children"

parent :: forall p. Lacks "children" p => ({ children :: Array JSX | p } -> JSX) -> Record p -> Markup Unit -> Markup Unit
parent el props kids = jsx [el $ Record.insert childrenProxy (getChildren kids) props]

child :: forall p. (Record p -> JSX) -> Record p -> Markup Unit
child el props = jsx [el props]

--touchableOpacity :: forall p. Lacks "children" p => Record p -> Markup JSX -> Markup JSX
touchableOpacity = parent RN.touchableOpacity

--text :: forall p. Lacks "children" p => Record p -> Markup JSX -> Markup JSX
text = parent RN.text

scrollView = parent RN.scrollView

flatList = child RN.flatList

safeAreaView = parent RN.safeAreaView

--view :: forall p. Lacks "children" p => Record p -> Markup -> Markup
view = parent RN.view

statusBar = child RN.statusBar

image = child RN.image

string s = jsx [RN.string s]

parentElement component = parent (H.element component)

childElement component = child (H.element component)

css :: forall css. { | css } -> CSS
css = unsafeCoerce
