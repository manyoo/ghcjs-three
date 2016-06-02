{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module GHCJS.Three.CameraHelper where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Object3D
import GHCJS.Three.Visible
import GHCJS.Three.GLNode
import GHCJS.Three.Line
import GHCJS.Three.Camera

newtype CameraHelper = CameraHelper {
    cameraHelper :: Line
    } deriving (ThreeJSVal, IsObject3D, Visible, IsGLNode)

foreign import javascript unsafe "new window['THREE']['CameraHelper']($1)"
    thr_mkCameraHelper :: JSVal -> Three JSVal

mkCameraHelper :: Camera -> Three CameraHelper
mkCameraHelper = fmap fromJSVal . thr_mkCameraHelper . toJSVal
