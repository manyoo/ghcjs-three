{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Camera where

import GHCJS.Three.Monad
import GHCJS.Three.Object3D

data CCamera a
type Camera a = Object3D (CCamera a)

data COrthographicCamera a
type OrthographicCamera a = Camera (COrthographicCamera a)

type Left = Double
type Right = Double
type Top = Double
type Bottom = Double
type Zoom = Double

data CPerspectiveCamera a
type PerspectiveCamera a = Camera (CPerspectiveCamera a)

type Fov = Double
type Aspect = Double
type Near = Double
type Far = Double
type FocalLength = Double
type FrameSize = Double

-- | create a new orthographic camera
foreign import javascript unsafe "new window.THREE.OrthographicCamera($1, $2, $3, $4, $5, $6)"
    mkOrthographicCamera :: Left -> Right -> Top -> Bottom -> Near -> Far -> Three (OrthographicCamera ())

-- | create a new perspective camera
foreign import javascript unsafe "new window.THREE.PerspectiveCamera($1, $2, $3, $4)"
    mkPerspectiveCamera :: Fov -> Aspect -> Near -> Far -> Three (PerspectiveCamera ())

-- | perspective camera operations

-- | get field of view
foreign import javascript safe "($1).fov"
    fov :: PerspectiveCamera a -> Fov

-- | set field of view
foreign import javascript unsafe "($2).fov = $1"
    setFov :: Fov -> PerspectiveCamera a -> Three ()

-- | get aspect
foreign import javascript safe "($1).aspect"
    aspect :: PerspectiveCamera a -> Aspect

-- | set aspect
foreign import javascript unsafe "($2).aspect = $1"
    setAspect :: Aspect -> PerspectiveCamera a -> Three ()

-- | set Lens
foreign import javascript unsafe "($3).setLens($1, $2)"
    setLens :: FocalLength -> FrameSize -> PerspectiveCamera a -> Three ()

-- | orthographic camera operations

-- | get left
foreign import javascript safe "($1).left"
    left :: OrthographicCamera a -> Left

-- | set left
foreign import javascript unsafe "($2).left = $1"
    setLeft :: Left -> OrthographicCamera a -> Three ()

-- | get right
foreign import javascript safe "($1).right"
    right :: OrthographicCamera a -> Right

-- | set right
foreign import javascript unsafe "($2).right = $1"
    setRight :: Right -> OrthographicCamera a -> Three ()

-- | get top
foreign import javascript safe "($1).top"
    top :: OrthographicCamera a -> Top

-- | set top
foreign import javascript unsafe "($2).top = $1"
    setTop :: Top -> OrthographicCamera a -> Three ()

-- | get bottom
foreign import javascript safe "($1).bottom"
    bottom :: OrthographicCamera a -> Bottom

-- | set bottom
foreign import javascript unsafe "($2).bottom = $1"
    setBottom :: Bottom -> OrthographicCamera a -> Three ()

-- | common camera operations
-- | get near
foreign import javascript safe "($1).near"
    near :: Camera a -> Near

-- | set near
foreign import javascript unsafe "($2).near = $1"
    setNear :: Near -> Camera a -> Three ()

-- | get far
foreign import javascript safe "($1).far"
    far :: Camera a -> Far

-- | set far
foreign import javascript unsafe "($2).far = $1"
    setFar :: Far -> Camera a -> Three ()

-- | update projection matrix
foreign import javascript unsafe "($1).updateProjectionMatrix()"
    updateProjectionMatrix :: Camera a -> Three ()
