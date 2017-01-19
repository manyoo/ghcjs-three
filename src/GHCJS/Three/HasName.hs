{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.HasName
    (HasName(..)
    ) where

import GHCJS.Types
import GHCJS.Three.Monad
import Data.JSString (pack, unpack)

-- | get name
foreign import javascript unsafe "($1)['name']"
    thr_getName :: JSVal -> Three JSString

-- | set name
foreign import javascript unsafe "($2)['name'] = $1"
    thr_setName :: JSString -> JSVal -> Three ()

class ThreeJSVal o => HasName o where
    getName :: o -> Three String
    getName o = unpack <$> thr_getName (toJSVal o)

    setName :: String -> o -> Three ()
    setName n o = thr_setName (pack n) (toJSVal o)
