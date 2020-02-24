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

newtype Markup a
  = Markup (Tuple JSX a)

instance semigroupMarkup :: Semigroup a => Semigroup (Markup a) where
  append a b = Markup (Tuple jsx value)
    where
    jsx = (getJsx a) <> (getJsx b)

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

getJsx :: forall a. Markup a -> JSX
getJsx (Markup (Tuple jsx a)) = jsx

runMarkup :: forall a. Markup a -> a
runMarkup (Markup (Tuple jsx a)) = a

jsx :: JSX -> Markup Unit
jsx j = Markup (Tuple (j) unit)

childrenProxy = SProxy :: SProxy "children"

parent :: forall p. Lacks "children" p => ({ children :: Array JSX | p } -> JSX) -> Record p -> Markup Unit -> Markup Unit
parent el props kids = jsx $ el $ Record.insert childrenProxy [ getJsx kids ] props

child :: forall p. (Record p -> JSX) -> Record p -> Markup Unit
child el props = jsx $ el props

wrappedChild :: forall p m. Functor m => (Record p -> m JSX) -> Record p -> m (Markup Unit)
wrappedChild el props = jsx <$> el props

--touchableOpacity :: forall p. Lacks "children" p => Record p -> Markup JSX -> Markup JSX
touchableOpacity = parent RN.touchableOpacity

--text :: forall p. Lacks "children" p => Record p -> Markup JSX -> Markup JSX
text = parent RN.text

safeAreaView = parent RN.safeAreaView

--view :: forall p. Lacks "children" p => Record p -> Markup -> Markup
view = parent RN.view

statusBar = child RN.statusBar

string s = jsx $ RN.string s

parentElement component = parent (H.element component)

childElement component = child (H.element component)

css :: forall css. { | css } -> CSS
css = unsafeCoerce
