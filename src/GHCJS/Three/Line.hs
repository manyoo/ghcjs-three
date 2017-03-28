{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Line (Line(..), mkLine, isLine) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Object3D
import GHCJS.Three.Geometry
import GHCJS.Three.Material
import GHCJS.Three.Visible
import GHCJS.Three.GLNode
import GHCJS.Three.HasGeoMat
import GHCJS.Three.HasName
import GHCJS.Three.CanCopy

-- | Line type definition
newtype Line = Line {
    lineObject3D :: Object3D
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode, CanCopy)

instance HasGeoMat Line
instance HasName Line

foreign import javascript unsafe "new window['THREE']['Line']($1, $2)"
    thr_mkLine :: JSVal -> JSVal -> Three JSVal

foreign import javascript unsafe "($1)['isLine']"
    thr_isLine :: JSVal -> Three Bool

-- | create a new Line with Geometry and line material
mkLine :: (IsGeometry g, IsLineMaterial m) => g -> m -> Three Line
mkLine g m = fromJSVal <$> thr_mkLine (toJSVal g) (toJSVal m)

isLine :: Line -> Three Bool
isLine = thr_isLine . toJSVal
