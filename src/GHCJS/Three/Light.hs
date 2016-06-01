{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Light (
    Light(..), mkLight,
    AmbientLight(..), mkAmbientLight,
    DirectionalLight(..), mkDirectionalLight,
    PointLight(..), mkPointLight,
    SpotLight(..), mkSpotLight
) where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Object3D
import GHCJS.Three.Visible
import GHCJS.Three.GLNode

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
