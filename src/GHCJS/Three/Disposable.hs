{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Disposable (Disposable(..))
    where

import GHCJS.Types
import GHCJS.Three.Monad

foreign import javascript unsafe "($1).dispose()"
    oDispose :: JSRef -> Three ()

class (ThreeJSRef o) => Disposable o where
    dispose :: o -> Three ()
    dispose = oDispose . toJSRef
