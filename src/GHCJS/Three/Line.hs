{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Line (Line(..), mkLine) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Object3D
import GHCJS.Three.Geometry
import GHCJS.Three.Material
import GHCJS.Three.Visible
import GHCJS.Three.GLNode

-- | Line type definition
newtype Line = Line {
    lineObject3D :: Object3D
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window.THREE.Line($1, $2)"
    thr_mkLine :: JSVal -> JSVal -> Three JSVal

-- | create a new Line with Geometry and line material
mkLine :: (IsGeometry g, IsLineMaterial m) => g -> m -> Three Line
mkLine g m = fromJSVal <$> thr_mkLine (toJSVal g) (toJSVal m)
