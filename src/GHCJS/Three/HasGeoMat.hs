{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.HasGeoMat
    ( HasGeoMat(..)
    ) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Geometry
import GHCJS.Three.Material

foreign import javascript unsafe "($1)['geometry']"
    thr_geometry :: JSVal -> Three JSVal

foreign import javascript unsafe "($2)['geometry'] = $1"
    thr_setGeometry :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($1)['material']"
    thr_material :: JSVal -> Three JSVal

foreign import javascript unsafe "($2)['material'] = $1"
    thr_setMaterial :: JSVal -> JSVal -> Three ()

class ThreeJSVal m => HasGeoMat m where
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
