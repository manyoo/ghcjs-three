{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Camera (
    Camera(..), IsCamera(..),
    OrthographicCamera(..), IsOrthoGraphicCamera(..),
    PerspectiveCamera(..), IsPerspectiveCamera(..),
    mkOrthographicCamera, mkPerspectiveCamera
    ) where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Object3D

newtype Camera = Camera {
    cameraObject3D :: Object3D
} deriving (ThreeJSRef, IsObject3D)

-- | common camera operations
-- | get near
foreign import javascript safe "($1).near"
    thr_near :: JSRef -> Near

-- | set near
foreign import javascript unsafe "($2).near = $1"
    thr_setNear :: Near -> JSRef -> Three ()

-- | get far
foreign import javascript safe "($1).far"
    thr_far :: JSRef -> Far

-- | set far
foreign import javascript unsafe "($2).far = $1"
    thr_setFar :: Far -> JSRef -> Three ()

-- | update projection matrix
foreign import javascript unsafe "($1).updateProjectionMatrix()"
    thr_updateProjectionMatrix :: JSRef -> Three ()

class (ThreeJSRef c) => IsCamera c where
    near :: c -> Near
    near = thr_near . toJSRef

    setNear :: Near -> c -> Three ()
    setNear n c = thr_setNear n $ toJSRef c

    far :: c -> Far
    far = thr_far . toJSRef

    setFar :: Far -> c -> Three ()
    setFar f c = thr_setFar f $ toJSRef c

    updateProjectionMatrix :: c -> Three ()
    updateProjectionMatrix = thr_updateProjectionMatrix . toJSRef

instance IsCamera Camera

-- | OrthographicCamera definition and APIs
newtype OrthographicCamera = OrthographicCamera {
    getOrthoCamera :: Camera
} deriving (ThreeJSRef, IsObject3D, IsCamera)

type Left = Double
type Right = Double
type Top = Double
type Bottom = Double
type Zoom = Double
type Near = Double
type Far = Double

-- | create a new orthographic camera
foreign import javascript unsafe "new window.THREE.OrthographicCamera($1, $2, $3, $4, $5, $6)"
    thr_mkOrthographicCamera :: Left -> Right -> Top -> Bottom -> Near -> Far -> Three JSRef

mkOrthographicCamera :: Left -> Right -> Top -> Bottom -> Near -> Far -> Three OrthographicCamera
mkOrthographicCamera l r t b n f = fromJSRef <$> thr_mkOrthographicCamera l r t b n f

-- | orthographic camera operations

-- | get left
foreign import javascript safe "($1).left"
    thr_left :: JSRef -> Left

-- | set left
foreign import javascript unsafe "($2).left = $1"
    thr_setLeft :: Left -> JSRef -> Three ()

-- | get right
foreign import javascript safe "($1).right"
    thr_right :: JSRef -> Right

-- | set right
foreign import javascript unsafe "($2).right = $1"
    thr_setRight :: Right -> JSRef -> Three ()

-- | get top
foreign import javascript safe "($1).top"
    thr_top :: JSRef -> Top

-- | set top
foreign import javascript unsafe "($2).top = $1"
    thr_setTop :: Top -> JSRef -> Three ()

-- | get bottom
foreign import javascript safe "($1).bottom"
    thr_bottom :: JSRef -> Bottom

-- | set bottom
foreign import javascript unsafe "($2).bottom = $1"
    thr_setBottom :: Bottom -> JSRef -> Three ()

class (ThreeJSRef c) => IsOrthoGraphicCamera c where
    left :: c -> Left
    left = thr_left . toJSRef

    setLeft :: Left -> c -> Three ()
    setLeft l c = thr_setLeft l $ toJSRef c

    right :: c -> Right
    right = thr_right . toJSRef

    setRight :: Right -> c -> Three ()
    setRight r c = thr_setRight r $ toJSRef c

    top :: c -> Top
    top = thr_top . toJSRef

    setTop :: Top -> c -> Three ()
    setTop t c = thr_setTop t $ toJSRef c

    bottom :: c -> Bottom
    bottom = thr_bottom . toJSRef

    setBottom :: Bottom -> c -> Three ()
    setBottom b c = thr_setBottom b $ toJSRef c

instance IsOrthoGraphicCamera OrthographicCamera

-- | PerspectiveCamera definition and APIs
newtype PerspectiveCamera = PerspectiveCamera {
    getPersCamera :: Camera
} deriving (ThreeJSRef, IsObject3D, IsCamera)

type Fov = Double
type Aspect = Double
type FocalLength = Double
type FrameSize = Double

-- | create a new perspective camera
foreign import javascript unsafe "new window.THREE.PerspectiveCamera($1, $2, $3, $4)"
    thr_mkPerspectiveCamera :: Fov -> Aspect -> Near -> Far -> Three JSRef

mkPerspectiveCamera :: Fov -> Aspect -> Near -> Far -> Three PerspectiveCamera
mkPerspectiveCamera fov a n f = fromJSRef <$> thr_mkPerspectiveCamera fov a n f

-- | perspective camera operations

-- | get field of view
foreign import javascript safe "($1).fov"
    thr_fov :: JSRef -> Fov

-- | set field of view
foreign import javascript unsafe "($2).fov = $1"
    thr_setFov :: Fov -> JSRef -> Three ()

-- | get aspect
foreign import javascript safe "($1).aspect"
    thr_aspect :: JSRef -> Aspect

-- | set aspect
foreign import javascript unsafe "($2).aspect = $1"
    thr_setAspect :: Aspect -> JSRef -> Three ()

-- | set Lens
foreign import javascript unsafe "($3).setLens($1, $2)"
    thr_setLens :: FocalLength -> FrameSize -> JSRef -> Three ()

class (ThreeJSRef c) => IsPerspectiveCamera c where
    fov :: c -> Fov
    fov = thr_fov . toJSRef

    setFov :: Fov -> c -> Three ()
    setFov f c = thr_setFov f $ toJSRef c

    aspect :: c -> Aspect
    aspect = thr_aspect . toJSRef

    setAspect :: Aspect -> c -> Three ()
    setAspect a c = thr_setAspect a $ toJSRef c

    setLens :: FocalLength -> FrameSize -> c -> Three ()
    setLens l s c = thr_setLens l s $ toJSRef c

instance IsPerspectiveCamera PerspectiveCamera
