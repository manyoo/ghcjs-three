{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Material (
    Material(..), mkMaterial, IsMaterial(..),
    MeshBasicMaterial(..), mkMeshBasicMaterial,
    MeshNormalMaterial(..), mkMeshNormalMaterial,
    MeshLambertMaterial(..), mkMeshLambertMaterial,
    MeshPhongMaterial(..), mkMeshPhongMaterial
) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Color
import GHCJS.Three.Disposable
import GHCJS.Three.Visible

-- | generic Material
newtype Material = Material {
    getObject :: Object
} deriving (ThreeJSRef)

foreign import javascript unsafe "new window.THREE.Material()"
    thr_mkMaterial :: Three JSRef

-- | create a new Material instance
mkMaterial :: Three Material
mkMaterial = fromJSRef <$> thr_mkMaterial

-- private imported functions
foreign import javascript safe "($1).opacity"
    thr_opacity :: JSRef -> Double

foreign import javascript unsafe "($2).opacity = $1"
    thr_setOpacity :: Double -> JSRef -> Three ()

foreign import javascript safe "($1).transparent"
    thr_transparent :: JSRef -> Bool

foreign import javascript unsafe "($2).transparent = $1"
    thr_setTransparent :: Bool -> JSRef -> Three ()

class ThreeJSRef m => IsMaterial m where
    -- | get opacity
    opacity :: m -> Double
    opacity = thr_opacity . toJSRef

    -- | set opacity
    setOpacity :: Double -> m -> Three ()
    setOpacity o m = thr_setOpacity o $ toJSRef m

    -- | get transparent
    transparent :: m -> Bool
    transparent = thr_transparent . toJSRef

    -- | set transparent
    setTransparent :: Bool -> m -> Three ()
    setTransparent t m = thr_setTransparent t $ toJSRef m

instance IsMaterial Material
instance HasColor Material
instance Disposable Material
instance Visible Material

-- | MeshBasicMaterial
newtype MeshBasicMaterial = MeshBasicMaterial {
    basicMaterial :: Material
} deriving (ThreeJSRef, IsMaterial, HasColor, Disposable, Visible)

foreign import javascript unsafe "new window.THREE.MeshBasicMaterial()"
    thr_mkMeshBasicMaterial :: Three JSRef

-- | create a new MeshBasicMaterial
mkMeshBasicMaterial :: Three MeshBasicMaterial
mkMeshBasicMaterial = fromJSRef <$> thr_mkMeshBasicMaterial

-- | MeshNormalMaterial
newtype MeshNormalMaterial = MeshNormalMaterial {
    normalMaterial :: Material
} deriving (ThreeJSRef, IsMaterial, HasColor, Disposable, Visible)

foreign import javascript unsafe "new window.THREE.MeshNormalMaterial()"
    thr_mkMeshNormalMaterial :: Three JSRef

-- | create a new MeshNormalMaterial
mkMeshNormalMaterial :: Three MeshNormalMaterial
mkMeshNormalMaterial = fromJSRef <$> thr_mkMeshNormalMaterial

-- | MeshLambertMaterial
newtype MeshLambertMaterial = MeshLambertMaterial {
    lambertMaterial :: Material
} deriving (ThreeJSRef, IsMaterial, HasColor, Disposable, Visible)

foreign import javascript unsafe "new window.THREE.MeshLambertMaterial()"
    thr_mkMeshLambertMaterial :: Three JSRef

-- | create a new MeshLambertMaterial
mkMeshLambertMaterial :: Three MeshLambertMaterial
mkMeshLambertMaterial = fromJSRef <$> thr_mkMeshLambertMaterial

-- | MeshPhongMaterial
newtype MeshPhongMaterial = MeshPhongMaterial {
    phongMaterial :: Material
} deriving (ThreeJSRef, IsMaterial, HasColor, Disposable, Visible)

foreign import javascript unsafe "new window.THREE.MeshPhongMaterial()"
    thr_mkMeshPhongMaterial :: Three JSRef

-- | create a new MeshPhongMaterial
mkMeshPhongMaterial :: Three MeshPhongMaterial
mkMeshPhongMaterial = fromJSRef <$> thr_mkMeshPhongMaterial
