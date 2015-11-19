module GHCJS.Three.Monad (
    Three,
    BaseObject(..),
    ThreeJSVal(..),
    (~:),
    toJSValsHelper
) where

import GHCJS.Types
import qualified GHCJS.Marshal as M

type Three = IO

newtype BaseObject = BaseObject JSVal

class ThreeJSVal o where
    toJSVal :: o -> JSVal
    fromJSVal :: JSVal -> o

instance ThreeJSVal BaseObject where
    toJSVal (BaseObject r) = r
    fromJSVal = BaseObject

-- | helper function to construct option values
c ~: v = c v

-- | a helper function used in this library for building JS objects easier
toJSValsHelper :: (M.ToJSVal v) => k -> v -> IO (k, JSVal)
toJSValsHelper k v = M.toJSVal v >>= return . (,) k
