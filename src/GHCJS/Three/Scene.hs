{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Scene (
    Scene(..), mkScene
) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Object3D

-- | scene definition
newtype Scene = Scene {
    sceneObject3D :: Object3D
} deriving (ThreeJSRef, IsObject3D)

foreign import javascript unsafe "new window.THREE.Scene()"
    thr_mkScene :: Three JSRef

-- | create a new Scene
mkScene :: Three Scene
mkScene = fromJSRef <$> thr_mkScene
