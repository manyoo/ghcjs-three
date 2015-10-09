{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Visible (
    Visible(..)
) where

import GHCJS.Types
import GHCJS.Three.Monad

foreign import javascript safe "($1).visible"
    thr_visible :: JSRef -> Bool

foreign import javascript unsafe "($2).visible = $1"
    thr_setVisible :: Bool -> JSRef -> Three ()

class ThreeJSRef v => Visible v where
    -- | get Visible
    visible :: v -> Bool
    visible = thr_visible . toJSRef

    -- | set visible
    setVisible :: Bool -> v -> Three ()
    setVisible b v = thr_setVisible b $ toJSRef v
