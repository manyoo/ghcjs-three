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
    materialObject :: Object
} deriving (ThreeJSVal)

foreign import javascript unsafe "new window.THREE.Material()"
    thr_mkMaterial :: Three JSVal

-- | create a new Material instance
mkMaterial :: Three Material
mkMaterial = fromJSVal <$> thr_mkMaterial

-- private imported functions
foreign import javascript safe "($1).opacity"
    thr_opacity :: JSVal -> Double

foreign import javascript unsafe "($2).opacity = $1"
    thr_setOpacity :: Double -> JSVal -> Three ()

foreign import javascript safe "($1).transparent"
    thr_transparent :: JSVal -> Bool

foreign import javascript unsafe "($2).transparent = $1"
    thr_setTransparent :: Bool -> JSVal -> Three ()

class ThreeJSVal m => IsMaterial m where
    -- | get opacity
    opacity :: m -> Double
    opacity = thr_opacity . toJSVal

    -- | set opacity
    setOpacity :: Double -> m -> Three ()
    setOpacity o m = thr_setOpacity o $ toJSVal m

    -- | get transparent
    transparent :: m -> Bool
    transparent = thr_transparent . toJSVal

    -- | set transparent
    setTransparent :: Bool -> m -> Three ()
    setTransparent t m = thr_setTransparent t $ toJSVal m

instance IsMaterial Material
instance HasColor Material
instance Disposable Material
instance Visible Material

-- | MeshBasicMaterial
newtype MeshBasicMaterial = MeshBasicMaterial {
    basicMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Disposable, Visible)

foreign import javascript unsafe "new window.THREE.MeshBasicMaterial()"
    thr_mkMeshBasicMaterial :: Three JSVal

-- | create a new MeshBasicMaterial
mkMeshBasicMaterial :: Three MeshBasicMaterial
mkMeshBasicMaterial = fromJSVal <$> thr_mkMeshBasicMaterial

-- | MeshNormalMaterial
newtype MeshNormalMaterial = MeshNormalMaterial {
    normalMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Disposable, Visible)

foreign import javascript unsafe "new window.THREE.MeshNormalMaterial()"
    thr_mkMeshNormalMaterial :: Three JSVal

-- | create a new MeshNormalMaterial
mkMeshNormalMaterial :: Three MeshNormalMaterial
mkMeshNormalMaterial = fromJSVal <$> thr_mkMeshNormalMaterial

-- | MeshLambertMaterial
newtype MeshLambertMaterial = MeshLambertMaterial {
    lambertMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Disposable, Visible)

foreign import javascript unsafe "new window.THREE.MeshLambertMaterial()"
    thr_mkMeshLambertMaterial :: Three JSVal

-- | create a new MeshLambertMaterial
mkMeshLambertMaterial :: Three MeshLambertMaterial
mkMeshLambertMaterial = fromJSVal <$> thr_mkMeshLambertMaterial

-- | MeshPhongMaterial
newtype MeshPhongMaterial = MeshPhongMaterial {
    phongMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Disposable, Visible)

foreign import javascript unsafe "new window.THREE.MeshPhongMaterial()"
    thr_mkMeshPhongMaterial :: Three JSVal

-- | create a new MeshPhongMaterial
mkMeshPhongMaterial :: Three MeshPhongMaterial
mkMeshPhongMaterial = fromJSVal <$> thr_mkMeshPhongMaterial
