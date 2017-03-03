{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.CanCopy (
    CanCopy(..)) where

import GHCJS.Types
import GHCJS.Three.Monad

foreign import javascript unsafe "($2)['copy']($1)"
    thr_copy :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($1)['clone']()"
    thr_clone :: JSVal -> Three JSVal

class ThreeJSVal v => CanCopy v where
    copyTo :: v -> v -> Three ()
    v1 `copyTo` v2 = thr_copy (toJSVal v1) (toJSVal v2)

    clone :: v -> Three v
    clone = fmap fromJSVal . thr_clone . toJSVal
