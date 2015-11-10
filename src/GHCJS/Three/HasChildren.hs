{-# LANGUAGE JavaScriptFFI, MultiParamTypeClasses #-}
module GHCJS.Three.HasChildren (
    HasChildren(..)
) where

import GHCJS.Types

import GHCJS.Three.Monad

foreign import javascript unsafe "($2).add($1)"
    thr_add :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2).remove($1)"
    thr_remove :: JSVal -> JSVal -> Three ()

class (ThreeJSVal p, ThreeJSVal c) => HasChildren p c where
    -- | add child object c to parent p
    add :: c -> p -> Three ()
    add c p = thr_add (toJSVal c) (toJSVal p)

    -- | remove child object c from parent p
    remove :: c -> p -> Three ()
    remove c p = thr_remove (toJSVal c) (toJSVal p)
