module GHCJS.Three.Monad (
    Three,
    Object(..),
    ThreeJSVal(..)
) where

import GHCJS.Types

type Three = IO

newtype Object = Object JSVal

class ThreeJSVal o where
    toJSVal :: o -> JSVal
    fromJSVal :: JSVal -> o

instance ThreeJSVal Object where
    toJSVal (Object r) = r
    fromJSVal = Object
