module Markup where
import Prelude
import Control.Monad.Free (Free, foldFree, hoistFree, liftF)
import React.Basic.DOM.Internal (CSS)
import React.Basic.Native as RN
import React.Basic.Hooks (JSX)
import Unsafe.Coerce (unsafeCoerce)
import Prim.Row (class Lacks)
import Data.Tuple
import Record as Record
import Data.Symbol (SProxy(..))
import React.Basic.Hooks as H

newtype Markup a = Markup (Tuple JSX a)

instance functorMarkup :: Functor (Markup)  where
  map f (Markup (Tuple jsx a)) = Markup (Tuple jsx (f a))
instance applyMarkup :: Apply (Markup) where
  apply (Markup (Tuple firstJsx fn)) (Markup (Tuple secondJsx a)) 
    = Markup (Tuple (firstJsx <> secondJsx) (fn a))
instance applicativeMarkup :: Applicative Markup where
  pure x = Markup (Tuple mempty x)

instance bindMarkup :: Bind (Markup) where
  bind (Markup (Tuple jsx a)) f = (Markup (Tuple (jsx <> newJsx) newA))
    where (Markup (Tuple newJsx newA)) = f a

instance monadMarkup :: Monad Markup

getJsx :: forall a. Markup a -> JSX
getJsx (Markup (Tuple jsx a)) = jsx

jsx :: JSX -> Markup Unit
jsx j = Markup (Tuple (j) unit)

childrenProxy = SProxy :: SProxy "children"
parent :: forall p. Lacks "children" p => ({children :: Array JSX | p} -> JSX) -> Record p -> Markup Unit -> Markup Unit
parent el props kids =
  jsx $ el $ Record.insert childrenProxy [getJsx kids] props

child :: forall p. (Record p -> JSX) -> Record p -> Markup Unit
child el props = jsx $ el props


--touchableOpacity :: forall p. Lacks "children" p => Record p -> Markup JSX -> Markup JSX
touchableOpacity = parent RN.touchableOpacity

--text :: forall p. Lacks "children" p => Record p -> Markup JSX -> Markup JSX
text = parent RN.text

--view :: forall p. Lacks "children" p => Record p -> Markup -> Markup
view = parent RN.view

statusBar = child RN.statusBar

string s = jsx $ RN.string s

element component = parent (H.element component)

css :: forall css. { | css } -> CSS
css = unsafeCoerce
