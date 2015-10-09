{-# LANGUAGE JavaScriptFFI, MultiParamTypeClasses #-}
module GHCJS.Three.HasChildren (
    HasChildren(..)
) where

import GHCJS.Types

import GHCJS.Three.Monad

foreign import javascript unsafe "($2).add($1)"
    thr_add :: JSRef -> JSRef -> Three ()

foreign import javascript unsafe "($2).remove($1)"
    thr_remove :: JSRef -> JSRef -> Three ()

class (ThreeJSRef p, ThreeJSRef c) => HasChildren p c where
    -- | add child object c to parent p
    add :: c -> p -> Three ()
    add c p = thr_add (toJSRef c) (toJSRef p)

    -- | remove child object c from parent p
    remove :: c -> p -> Three ()
    remove c p = thr_remove (toJSRef c) (toJSRef p)
