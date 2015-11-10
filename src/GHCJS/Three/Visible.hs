{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Visible (
    Visible(..)
) where

import GHCJS.Types
import GHCJS.Three.Monad

foreign import javascript safe "($1).visible"
    thr_visible :: JSVal -> Bool

foreign import javascript unsafe "($2).visible = $1"
    thr_setVisible :: Bool -> JSVal -> Three ()

class ThreeJSVal v => Visible v where
    -- | get Visible
    visible :: v -> Bool
    visible = thr_visible . toJSVal

    -- | set visible
    setVisible :: Bool -> v -> Three ()
    setVisible b v = thr_setVisible b $ toJSVal v
