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
import GHCJS.Three.GLNode

-- | Mesh type definition
newtype Mesh = Mesh {
    meshObject3D :: Object3D
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window['THREE']['Mesh']($1, $2)"
    thr_mkMesh :: JSVal -> JSVal -> Three JSVal

-- | create a new Mesh with Geometry and Material
mkMesh :: (IsGeometry g, IsMaterial m) => g -> m -> Three Mesh
mkMesh g m = fromJSVal <$> thr_mkMesh (toJSVal g) (toJSVal m)

foreign import javascript safe "($1)['geometry']"
    thr_geometry :: JSVal -> Three JSVal

foreign import javascript unsafe "($2)['geometry'] = $1"
    thr_setGeometry :: JSVal -> JSVal -> Three ()

foreign import javascript safe "($1)['material']"
    thr_material :: JSVal -> Three JSVal

foreign import javascript unsafe "($2)['material'] = $1"
    thr_setMaterial :: JSVal -> JSVal -> Three ()

class ThreeJSVal m => IsMesh m where
    -- | get geometry
    geometry :: m -> Three Geometry
    geometry = fmap fromJSVal . thr_geometry . toJSVal

    -- | set geometry
    setGeometry :: IsGeometry g => g -> m -> Three ()
    setGeometry g m = thr_setGeometry (toJSVal g) (toJSVal m)

    -- | get material
    material :: m -> Three Material
    material = fmap fromJSVal . thr_material . toJSVal

    -- | set material
    setMaterial :: IsMaterial mat => mat -> m -> Three ()
    setMaterial mat m = thr_setMaterial (toJSVal mat) (toJSVal m)

instance IsMesh Mesh
