{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Visible (
    Visible(..)
) where

import GHCJS.Types
import GHCJS.Three.Monad

foreign import javascript safe "($1)['visible']"
    thr_visible :: JSVal -> Bool

-- NOTE: ghcjs/ghc has a bug that Bool values are not translated to JS
-- correctly at the time. So we use an Int here to bypass the bug
foreign import javascript unsafe "($2)['visible'] = $1 === 1"
    thr_setVisible :: Int -> JSVal -> Three ()

class ThreeJSVal v => Visible v where
    -- | get Visible
    visible :: v -> Bool
    visible = thr_visible . toJSVal

    -- | set visible
    setVisible :: Bool -> v -> Three ()
    setVisible b v = thr_setVisible (if b then 1 else 0) $ toJSVal v
