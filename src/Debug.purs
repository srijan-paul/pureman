module Debug (debugger, log) where

foreign import debugger :: forall a. a -> a
foreign import log :: forall a. a -> a

