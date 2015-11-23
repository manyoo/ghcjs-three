{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Camera (
    Camera(..), IsCamera(..),
    OrthographicCamera(..), IsOrthoGraphicCamera(..),
    PerspectiveCamera(..), IsPerspectiveCamera(..),
    Left, Right, Top, Bottom, Fov, Aspect, Near, Far, Zoom, FocalLength, FrameSize,
    mkOrthographicCamera, mkPerspectiveCamera
    ) where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Object3D

newtype Camera = Camera {
    cameraObject3D :: Object3D
} deriving (ThreeJSVal, IsObject3D)

-- | common camera operations
-- | get near
foreign import javascript safe "($1).near"
    thr_near :: JSVal -> Near

-- | set near
foreign import javascript unsafe "($2).near = $1"
    thr_setNear :: Near -> JSVal -> Three ()

-- | get far
foreign import javascript safe "($1).far"
    thr_far :: JSVal -> Far

-- | set far
foreign import javascript unsafe "($2).far = $1"
    thr_setFar :: Far -> JSVal -> Three ()

-- | get zoom
foreign import javascript safe "($1).zoom"
    thr_zoom :: JSVal -> Zoom

-- | set zoom
foreign import javascript unsafe "($2).zoom = $1"
    thr_setZoom :: Zoom -> JSVal -> Three ()

-- | update projection matrix
foreign import javascript unsafe "($1).updateProjectionMatrix()"
    thr_updateProjectionMatrix :: JSVal -> Three ()

class (ThreeJSVal c) => IsCamera c where
    near :: c -> Near
    near = thr_near . toJSVal

    setNear :: Near -> c -> Three ()
    setNear n c = thr_setNear n $ toJSVal c

    far :: c -> Far
    far = thr_far . toJSVal

    setFar :: Far -> c -> Three ()
    setFar f c = thr_setFar f $ toJSVal c

    zoom :: c -> Zoom
    zoom = thr_zoom . toJSVal

    setZoom :: Zoom -> c -> Three ()
    setZoom z c = thr_setZoom z $ toJSVal c

    updateProjectionMatrix :: c -> Three ()
    updateProjectionMatrix = thr_updateProjectionMatrix . toJSVal

instance IsCamera Camera

-- | OrthographicCamera definition and APIs
newtype OrthographicCamera = OrthographicCamera {
    getOrthoCamera :: Camera
} deriving (ThreeJSVal, IsObject3D, IsCamera)

type Left = Double
type Right = Double
type Top = Double
type Bottom = Double
type Zoom = Double
type Near = Double
type Far = Double

-- | create a new orthographic camera
foreign import javascript unsafe "new window.THREE.OrthographicCamera($1, $2, $3, $4, $5, $6)"
    thr_mkOrthographicCamera :: Left -> Right -> Top -> Bottom -> Near -> Far -> Three JSVal

mkOrthographicCamera :: Left -> Right -> Top -> Bottom -> Near -> Far -> Three OrthographicCamera
mkOrthographicCamera l r t b n f = fromJSVal <$> thr_mkOrthographicCamera l r t b n f

-- | orthographic camera operations

-- | get left
foreign import javascript safe "($1).left"
    thr_left :: JSVal -> Left

-- | set left
foreign import javascript unsafe "($2).left = $1"
    thr_setLeft :: Left -> JSVal -> Three ()

-- | get right
foreign import javascript safe "($1).right"
    thr_right :: JSVal -> Right

-- | set right
foreign import javascript unsafe "($2).right = $1"
    thr_setRight :: Right -> JSVal -> Three ()

-- | get top
foreign import javascript safe "($1).top"
    thr_top :: JSVal -> Top

-- | set top
foreign import javascript unsafe "($2).top = $1"
    thr_setTop :: Top -> JSVal -> Three ()

-- | get bottom
foreign import javascript safe "($1).bottom"
    thr_bottom :: JSVal -> Bottom

-- | set bottom
foreign import javascript unsafe "($2).bottom = $1"
    thr_setBottom :: Bottom -> JSVal -> Three ()

class (ThreeJSVal c) => IsOrthoGraphicCamera c where
    left :: c -> Left
    left = thr_left . toJSVal

    setLeft :: Left -> c -> Three ()
    setLeft l c = thr_setLeft l $ toJSVal c

    right :: c -> Right
    right = thr_right . toJSVal

    setRight :: Right -> c -> Three ()
    setRight r c = thr_setRight r $ toJSVal c

    top :: c -> Top
    top = thr_top . toJSVal

    setTop :: Top -> c -> Three ()
    setTop t c = thr_setTop t $ toJSVal c

    bottom :: c -> Bottom
    bottom = thr_bottom . toJSVal

    setBottom :: Bottom -> c -> Three ()
    setBottom b c = thr_setBottom b $ toJSVal c

instance IsOrthoGraphicCamera OrthographicCamera

-- | PerspectiveCamera definition and APIs
newtype PerspectiveCamera = PerspectiveCamera {
    getPersCamera :: Camera
} deriving (ThreeJSVal, IsObject3D, IsCamera)

type Fov = Double
type Aspect = Double
type FocalLength = Double
type FrameSize = Double

-- | create a new perspective camera
foreign import javascript unsafe "new window.THREE.PerspectiveCamera($1, $2, $3, $4)"
    thr_mkPerspectiveCamera :: Fov -> Aspect -> Near -> Far -> Three JSVal

mkPerspectiveCamera :: Fov -> Aspect -> Near -> Far -> Three PerspectiveCamera
mkPerspectiveCamera fov a n f = fromJSVal <$> thr_mkPerspectiveCamera fov a n f

-- | perspective camera operations

-- | get field of view
foreign import javascript safe "($1).fov"
    thr_fov :: JSVal -> Fov

-- | set field of view
foreign import javascript unsafe "($2).fov = $1"
    thr_setFov :: Fov -> JSVal -> Three ()

-- | get aspect
foreign import javascript safe "($1).aspect"
    thr_aspect :: JSVal -> Aspect

-- | set aspect
foreign import javascript unsafe "($2).aspect = $1"
    thr_setAspect :: Aspect -> JSVal -> Three ()

-- | set Lens
foreign import javascript unsafe "($3).setLens($1, $2)"
    thr_setLens :: FocalLength -> FrameSize -> JSVal -> Three ()

class (ThreeJSVal c) => IsPerspectiveCamera c where
    fov :: c -> Fov
    fov = thr_fov . toJSVal

    setFov :: Fov -> c -> Three ()
    setFov f c = thr_setFov f $ toJSVal c

    aspect :: c -> Aspect
    aspect = thr_aspect . toJSVal

    setAspect :: Aspect -> c -> Three ()
    setAspect a c = thr_setAspect a $ toJSVal c

    setLens :: FocalLength -> FrameSize -> c -> Three ()
    setLens l s c = thr_setLens l s $ toJSVal c

instance IsPerspectiveCamera PerspectiveCamera
