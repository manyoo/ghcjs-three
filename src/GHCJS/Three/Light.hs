{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Light (
    Light(..), mkLight,
    AmbientLight(..), mkAmbientLight
) where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Object3D

-- | generic Light

newtype Light = Light {
    lightObject3D :: Object3D
} deriving (ThreeJSVal, IsObject3D)

foreign import javascript unsafe "new window.THREE.Light($1)"
    thr_mkLight :: Int -> Three JSVal

-- | create a new Light instance
mkLight :: Int -> Three Light
mkLight c = fromJSVal <$> thr_mkLight c


-- | Ambient Light
newtype AmbientLight = AmbientLight {
    getLight :: Light
} deriving (ThreeJSVal, IsObject3D)

foreign import javascript unsafe "new window.THREE.AmbientLight($1)"
    thr_mkAmbientLight :: Int -> Three JSVal

-- | create a new ambient light
mkAmbientLight :: Int -> Three AmbientLight
mkAmbientLight c = fromJSVal <$> thr_mkAmbientLight c
