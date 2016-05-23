{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Scene (
    Scene(..), mkScene
) where

import GHCJS.Types
import GHCJS.Three.Monad
import GHCJS.Three.Object3D
import GHCJS.Three.Visible
import GHCJS.Three.GLNode

-- | scene definition
newtype Scene = Scene {
    sceneObject3D :: Object3D
} deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window['THREE']['Scene']()"
    thr_mkScene :: Three JSVal

-- | create a new Scene
mkScene :: Three Scene
mkScene = fromJSVal <$> thr_mkScene
