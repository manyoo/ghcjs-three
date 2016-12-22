{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, FlexibleInstances, UndecidableInstances #-}
module GHCJS.Three.Material (
    Material(..), mkMaterial, IsMaterial(..),
    MaterialRenderFace, materialFrontSide, materialBackSide, materialDoubleSide,
    MeshBasicMaterial(..), mkMeshBasicMaterial, setWireFrame, setWireFrameLineWidth,
    MeshNormalMaterial(..), mkMeshNormalMaterial,
    MeshLambertMaterial(..), mkMeshLambertMaterial,
    MeshPhongMaterial(..), mkMeshPhongMaterial,
    TexturedMaterial(..),
    LineBasicMaterial(..), mkLineBasicMaterial,
    LineDashedMaterial(..), mkLineDashedMaterial,
    LineMaterial(..), IsLineMaterial(..)
) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Color
import GHCJS.Three.Texture
import GHCJS.Three.Disposable
import GHCJS.Three.Visible

-- | generic Material
newtype Material = Material {
    materialObject :: BaseObject
} deriving (ThreeJSVal)

instance HasColor Material
instance Visible Material
instance Disposable Material

foreign import javascript unsafe "new window['THREE']['Material']()"
    thr_mkMaterial :: Three JSVal

-- | create a new Material instance
mkMaterial :: Three Material
mkMaterial = fromJSVal <$> thr_mkMaterial

-- private imported functions
foreign import javascript safe "($1)['opacity']"
    thr_opacity :: JSVal -> Three Double

foreign import javascript unsafe "($2)['opacity'] = $1"
    thr_setOpacity :: Double -> JSVal -> Three ()

foreign import javascript safe "($1)['transparent']"
    thr_transparent :: JSVal -> Three Bool

foreign import javascript unsafe "($2)['transparent'] = $1 === 1"
    thr_setTransparent :: Int -> JSVal -> Three ()


type MaterialRenderFace = Int

foreign import javascript safe "window['THREE']['FrontSide']"
    materialFrontSide :: MaterialRenderFace

foreign import javascript safe "window['THREE']['BackSide']"
    materialBackSide :: MaterialRenderFace

foreign import javascript safe "window['THREE']['DoubleSide']"
    materialDoubleSide :: MaterialRenderFace


foreign import javascript safe "($1)['side']"
    thr_side :: JSVal -> Three MaterialRenderFace

foreign import javascript unsafe "($2)['side'] = $1"
    thr_setSide :: MaterialRenderFace -> JSVal -> Three ()

class ThreeJSVal m => IsMaterial m where
    toMaterial :: m -> Material
    toMaterial = fromJSVal . toJSVal

    fromMaterial :: Material -> m
    fromMaterial = fromJSVal . toJSVal

    -- | get opacity
    opacity :: m -> Three Double
    opacity = thr_opacity . toJSVal

    -- | set opacity
    setOpacity :: Double -> m -> Three ()
    setOpacity o m = thr_setOpacity o $ toJSVal m

    -- | get transparent
    transparent :: m -> Three Bool
    transparent = thr_transparent . toJSVal

    -- | set transparent
    setTransparent :: Bool -> m -> Three ()
    setTransparent t m = thr_setTransparent (if t then 1 else 0) $ toJSVal m

    side :: m -> Three MaterialRenderFace
    side = thr_side . toJSVal

    setSide :: MaterialRenderFace -> m -> Three ()
    setSide s m = thr_setSide s $ toJSVal m

instance IsMaterial Material

foreign import javascript unsafe "($2)['wireframe'] = $1 === 1"
    thr_setWireFrame :: Int -> JSVal -> Three ()

setWireFrame :: IsMaterial m => Bool -> m -> Three ()
setWireFrame b mesh = thr_setWireFrame (if b then 1 else 0) $ toJSVal mesh

foreign import javascript unsafe "($2)['wireframeLineWidth'] = $1"
    thr_setWireFrameLineWidth :: Int -> JSVal -> Three ()

setWireFrameLineWidth :: IsMaterial m => Int -> m -> Three ()
setWireFrameLineWidth w mesh = thr_setWireFrameLineWidth w $ toJSVal mesh

-- | MeshBasicMaterial
newtype MeshBasicMaterial = MeshBasicMaterial {
    basicMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, Visible, HasColor, Disposable)

foreign import javascript unsafe "new window['THREE']['MeshBasicMaterial']()"
    thr_mkMeshBasicMaterial :: Three JSVal

-- | create a new MeshBasicMaterial
mkMeshBasicMaterial :: Three MeshBasicMaterial
mkMeshBasicMaterial = fromJSVal <$> thr_mkMeshBasicMaterial

-- | MeshNormalMaterial
newtype MeshNormalMaterial = MeshNormalMaterial {
    normalMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Visible, Disposable)

foreign import javascript unsafe "new window['THREE']['MeshNormalMaterial']()"
    thr_mkMeshNormalMaterial :: Three JSVal

-- | create a new MeshNormalMaterial
mkMeshNormalMaterial :: Three MeshNormalMaterial
mkMeshNormalMaterial = fromJSVal <$> thr_mkMeshNormalMaterial


-- | class for materials that can set textures
foreign import javascript unsafe "($2)['map'] = $1"
    thr_setTextureMap :: JSVal -> JSVal -> Three ()

class IsMaterial m => TexturedMaterial m where
    setTextureMap :: Texture -> m -> Three ()
    setTextureMap t m = thr_setTextureMap (toJSVal t) (toJSVal m)


-- | MeshLambertMaterial
newtype MeshLambertMaterial = MeshLambertMaterial {
    lambertMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Visible, Disposable)

instance TexturedMaterial MeshLambertMaterial

foreign import javascript unsafe "new window['THREE']['MeshLambertMaterial']()"
    thr_mkMeshLambertMaterial :: Three JSVal

-- | create a new MeshLambertMaterial
mkMeshLambertMaterial :: Three MeshLambertMaterial
mkMeshLambertMaterial = fromJSVal <$> thr_mkMeshLambertMaterial

-- | MeshPhongMaterial
newtype MeshPhongMaterial = MeshPhongMaterial {
    phongMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Visible, Disposable)

instance TexturedMaterial MeshPhongMaterial

foreign import javascript unsafe "new window['THREE']['MeshPhongMaterial']()"
    thr_mkMeshPhongMaterial :: Three JSVal

-- | create a new MeshPhongMaterial
mkMeshPhongMaterial :: Three MeshPhongMaterial
mkMeshPhongMaterial = fromJSVal <$> thr_mkMeshPhongMaterial

-- | LineBasicMaterial
newtype LineBasicMaterial = LineBasicMaterial {
    lineBasicMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Visible, Disposable)

foreign import javascript unsafe "new window['THREE']['LineBasicMaterial']()"
    thr_mkLineBasicMaterial :: Three JSVal

mkLineBasicMaterial :: Three LineBasicMaterial
mkLineBasicMaterial = fromJSVal <$> thr_mkLineBasicMaterial

-- | LineDashedMaterial
newtype LineDashedMaterial = LineDashedMaterial {
    lineDashedMaterial :: Material
} deriving (ThreeJSVal, IsMaterial, HasColor, Visible, Disposable)

foreign import javascript unsafe "new window['THREE']['LineDashedMaterial']()"
    thr_mkLineDashedMaterial :: Three JSVal

mkLineDashedMaterial :: Three LineDashedMaterial
mkLineDashedMaterial = fromJSVal <$> thr_mkLineDashedMaterial

-- private functions
foreign import javascript unsafe "($1)['linewidth']"
    thr_lineWidth :: JSVal -> Three Int

foreign import javascript unsafe "($2)['linewidth'] = $1"
    thr_setLineWidth :: Int -> JSVal -> Three ()

newtype LineMaterial = LineMaterial Material deriving (ThreeJSVal, IsMaterial)

class (ThreeJSVal l, IsMaterial l) => IsLineMaterial l where
    toLineMaterial :: l -> LineMaterial
    toLineMaterial = fromMaterial . toMaterial

    fromLineMaterial :: LineMaterial -> l
    fromLineMaterial = fromMaterial . toMaterial

    lineWidth :: l -> Three Int
    lineWidth = thr_lineWidth . toJSVal

    setLineWidth :: Int -> l -> Three ()
    setLineWidth w l = thr_setLineWidth w $ toJSVal l

instance IsLineMaterial LineMaterial
instance IsLineMaterial LineBasicMaterial
instance IsLineMaterial LineDashedMaterial
