module Platform where

foreign import select :: forall a . {ios :: a, android :: a} -> a
