module BindThis where

foreign import bindThis :: forall a b . a -> b -> a
