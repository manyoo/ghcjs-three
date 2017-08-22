{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Light (
    Light(..), mkLight,
    AmbientLight(..), mkAmbientLight,
    DirectionalLight(..), mkDirectionalLight, shadowCamera, lightTarget,
    setShadowCameraLeft, setShadowCameraRight,
    setShadowCameraTop, setShadowCameraBottom,
    setShadowCameraNear, setShadowCameraFar,
    PointLight(..), mkPointLight,
    SpotLight(..), mkSpotLight
) where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Object3D
import GHCJS.Three.Visible
import GHCJS.Three.GLNode
import GHCJS.Three.Camera

-- | generic Light

newtype Light = Light {
    lightObject3D :: Object3D
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window['THREE']['Light']($1)"
    thr_mkLight :: Int -> Three JSVal

-- | create a new Light instance
mkLight :: Int -> Three Light
mkLight c = fromJSVal <$> thr_mkLight c


-- | Ambient Light
newtype AmbientLight = AmbientLight {
    getLight :: Light
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window['THREE']['AmbientLight']($1)"
    thr_mkAmbientLight :: Int -> Three JSVal

-- | create a new ambient light
mkAmbientLight :: Int -> Three AmbientLight
mkAmbientLight c = fromJSVal <$> thr_mkAmbientLight c


-- | Directional Light
newtype DirectionalLight = DirectionalLight {
    getDirectionLight :: Light
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window['THREE']['DirectionalLight']($1, $2)"
    thr_mkDirectionalLight :: Int -> Double -> Three JSVal

-- | create a new directional light
mkDirectionalLight :: Int -> Double -> Three DirectionalLight
mkDirectionalLight c i = fromJSVal <$> thr_mkDirectionalLight c i

foreign import javascript unsafe "$1['shadow']['camera']"
    thr_shadowCamera :: JSVal -> Three JSVal

shadowCamera :: DirectionalLight -> Three Camera
shadowCamera = fmap fromJSVal . thr_shadowCamera . toJSVal

foreign import javascript unsafe "$1['target']"
    thr_target :: JSVal -> Three JSVal

lightTarget :: DirectionalLight -> Three Object3D
lightTarget = fmap fromJSVal . thr_target . toJSVal

foreign import javascript unsafe "$2['shadow']['camera']['left'] = $1"
    thr_setShadowCameraLeft :: Double -> JSVal -> Three ()

setShadowCameraLeft :: Double -> DirectionalLight -> Three ()
setShadowCameraLeft d l = thr_setShadowCameraLeft d (toJSVal l)

foreign import javascript unsafe "$2['shadow']['camera']['right'] = $1"
    thr_setShadowCameraRight :: Double -> JSVal -> Three ()

setShadowCameraRight :: Double -> DirectionalLight -> Three ()
setShadowCameraRight d l = thr_setShadowCameraRight d (toJSVal l)

foreign import javascript unsafe "$2['shadow']['camera']['top'] = $1"
    thr_setShadowCameraTop :: Double -> JSVal -> Three ()

setShadowCameraTop :: Double -> DirectionalLight -> Three ()
setShadowCameraTop d l = thr_setShadowCameraTop d (toJSVal l)

foreign import javascript unsafe "$2['shadow']['camera']['bottom'] = $1"
    thr_setShadowCameraBottom :: Double -> JSVal -> Three ()

setShadowCameraBottom :: Double -> DirectionalLight -> Three ()
setShadowCameraBottom d l = thr_setShadowCameraBottom d (toJSVal l)

foreign import javascript unsafe "$2['shadow']['camera']['near'] = $1"
    thr_setShadowCameraNear :: Double -> JSVal -> Three ()

setShadowCameraNear :: Double -> DirectionalLight -> Three ()
setShadowCameraNear d l = thr_setShadowCameraNear d (toJSVal l)

foreign import javascript unsafe "$2['shadow']['camera']['far'] = $1"
    thr_setShadowCameraFar :: Double -> JSVal -> Three ()

setShadowCameraFar :: Double -> DirectionalLight -> Three ()
setShadowCameraFar d l = thr_setShadowCameraFar d (toJSVal l)

-- | Point Light
newtype PointLight = PointLight {
    getPointLight :: Light
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window['THREE']['PointLight']($1, $2, $3)"
    thr_mkPointLight :: Int -> Double -> Double -> Three JSVal

-- | create a new point light
mkPointLight :: Int -> Double -> Double -> Three PointLight
mkPointLight c i d = fromJSVal <$> thr_mkPointLight c i d


-- | Spot Light
newtype SpotLight = SpotLight {
    getSpotLight :: Light
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window['THREE']['SpotLight']($1, $2, $3)"
    thr_mkSpotLight :: Int -> Double -> Double -> Three JSVal

mkSpotLight :: Int -> Double -> Double -> Three SpotLight
mkSpotLight c i d = fromJSVal <$> thr_mkSpotLight c i d
