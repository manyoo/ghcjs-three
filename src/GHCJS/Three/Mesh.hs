{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Mesh (
    Mesh(..), mkMesh, IsMesh(..)
) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Object3D (Object3D, IsObject3D)
import GHCJS.Three.Geometry
import GHCJS.Three.Material
import GHCJS.Three.Visible
import GHCJS.Three.HasName
import GHCJS.Three.GLNode
import GHCJS.Three.HasGeoMat
import GHCJS.Three.CanCopy

-- | Mesh type definition
newtype Mesh = Mesh {
    meshObject3D :: Object3D
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode, HasName, CanCopy)

foreign import javascript unsafe "new window['THREE']['Mesh']($1, $2)"
    thr_mkMesh :: JSVal -> JSVal -> Three JSVal

-- | create a new Mesh with Geometry and Material
mkMesh :: (IsGeometry g, IsMaterial m) => g -> m -> Three Mesh
mkMesh g m = fromJSVal <$> thr_mkMesh (toJSVal g) (toJSVal m)

foreign import javascript unsafe "($1)['isMesh']"
    thr_isMesh :: JSVal -> Three Bool

class ThreeJSVal m => IsMesh m where
    isMesh :: m -> Three Bool
    isMesh = thr_isMesh . toJSVal

instance IsMesh Mesh
instance HasGeoMat Mesh
