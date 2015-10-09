{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Light (
    Light(..), mkLight,
    AmbientLight(..), mkAmbientLight
) where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Object3D

-- | generic Light

newtype Light = Light {
    getObject3D :: Object3D
}

instance ThreeJSRef Light where
    toJSRef = toJSRef . getObject3D
    fromJSRef = Light . fromJSRef

foreign import javascript unsafe "new window.THREE.Light($1)"
    thr_mkLight :: Int -> Three JSRef

-- | create a new Light instance
mkLight :: Int -> Three Light
mkLight c = fromJSRef <$> thr_mkLight c


-- | Ambient Light
newtype AmbientLight = AmbientLight {
    getLight :: Light
}

instance ThreeJSRef AmbientLight where
    toJSRef = toJSRef . getLight
    fromJSRef = AmbientLight . fromJSRef

foreign import javascript unsafe "new window.THREE.AmbientLight($1)"
    thr_mkAmbientLight :: Int -> Three JSRef

-- | create a new ambient light
mkAmbientLight :: Int -> Three AmbientLight
mkAmbientLight c = fromJSRef <$> thr_mkAmbientLight c
