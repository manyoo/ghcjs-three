{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Mesh (
    Mesh(..), mkMesh, IsMesh(..)
) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Object3D (Object3D)
import GHCJS.Three.Geometry
import GHCJS.Three.Material

-- | Mesh type definition
newtype Mesh = Mesh {
    getObject3D :: Object3D
} deriving (ThreeJSRef)

foreign import javascript unsafe "new window.THREE.Mesh($1, $2)"
    thr_mkMesh :: JSRef -> JSRef -> Three JSRef

-- | create a new Mesh with Geometry and Material
mkMesh :: (IsGeometry g, IsMaterial m) => g -> m -> Three Mesh
mkMesh g m = fromJSRef <$> thr_mkMesh (toJSRef g) (toJSRef m)

foreign import javascript safe "($1).geometry"
    thr_geometry :: JSRef -> JSRef

foreign import javascript unsafe "($2).geometry = $1"
    thr_setGeometry :: JSRef -> JSRef -> Three ()

foreign import javascript safe "($1).material"
    thr_material :: JSRef -> JSRef

foreign import javascript unsafe "($2).material = $1"
    thr_setMaterial :: JSRef -> JSRef -> Three ()

class ThreeJSRef m => IsMesh m where
    -- | get geometry
    geometry :: m -> Geometry
    geometry = fromJSRef . thr_geometry . toJSRef

    -- | set geometry
    setGeometry :: IsGeometry g => g -> m -> Three ()
    setGeometry g m = thr_setGeometry (toJSRef g) (toJSRef m)

    -- | get material
    material :: m -> Material
    material = fromJSRef . thr_material . toJSRef

    -- | set material
    setMaterial :: IsMaterial mat => mat -> m -> Three ()
    setMaterial mat m = thr_setMaterial (toJSRef mat) (toJSRef m)

instance IsMesh Mesh
