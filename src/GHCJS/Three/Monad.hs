module GHCJS.Three.Monad (
    Three,
    BaseObject(..),
    ThreeJSVal(..)
) where

import GHCJS.Types

type Three = IO

newtype BaseObject = BaseObject JSVal

class ThreeJSVal o where
    toJSVal :: o -> JSVal
    fromJSVal :: JSVal -> o

instance ThreeJSVal BaseObject where
    toJSVal (BaseObject r) = r
    fromJSVal = BaseObject
