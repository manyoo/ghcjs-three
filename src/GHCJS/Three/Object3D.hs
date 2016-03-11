{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module GHCJS.Three.Object3D (
    Object3D(..),
    mkObject3D,
    IsObject3D(..)
    ) where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Vector
import GHCJS.Three.Matrix
import GHCJS.Three.Euler
import GHCJS.Three.GLNode
import GHCJS.Three.Visible

newtype Object3D = Object3D {
    getObject :: BaseObject
} deriving (ThreeJSVal)

instance IsGLNode Object3D
instance Visible Object3D

-- | create a new Object3D object
foreign import javascript unsafe "new window.THREE.Object3D()"
    thr_mkObject3D :: Three JSVal

mkObject3D :: Three Object3D
mkObject3D = fromJSVal <$> thr_mkObject3D

-- | get scale
foreign import javascript safe "($1).scale"
    thr_scale :: JSVal -> JSVal

-- | set scale
foreign import javascript unsafe "($2).scale.copy($1)"
    thr_setScale :: JSVal -> JSVal -> Three ()

-- | get position
foreign import javascript safe "($1).position"
    thr_position :: JSVal -> JSVal

-- | set position
foreign import javascript unsafe "($2).position.copy($1)"
    thr_setPosition :: JSVal -> JSVal -> Three ()

-- | get rotation
foreign import javascript safe "($1).rotation"
    thr_rotation :: JSVal -> JSVal

-- | set rotation
foreign import javascript unsafe "($2).rotation.copy($1)"
    thr_setRotation :: JSVal -> JSVal -> Three ()

-- | get up direction
foreign import javascript safe "($1).up"
    thr_up :: JSVal -> JSVal

-- | set up direction
foreign import javascript unsafe "($2).up.copy($1)"
    thr_setUp :: JSVal -> JSVal -> Three ()

-- | set renderOrder
foreign import javascript unsafe "($2).renderOrder = $1"
    thr_setRenderOrder :: Int -> JSVal -> Three ()

-- | translate along the X, Y, Z axises
foreign import javascript unsafe "($2).translateX($1)"
    thr_translateX :: Double -> JSVal -> Three ()
foreign import javascript unsafe "($2).translateY($1)"
    thr_translateY :: Double -> JSVal -> Three ()
foreign import javascript unsafe "($2).translateZ($1)"
    thr_translateZ :: Double -> JSVal -> Three ()

-- | translate along an axis, the axis should be a normalized vector
foreign import javascript unsafe "($3).translateOnAxis($1, $2)"
    thr_translateOnAxis :: JSVal -> Double -> JSVal -> Three ()

-- | translate between local and world coordinates
foreign import javascript unsafe "($2).localToWorld($1)"
    thr_localToWorld :: JSVal -> JSVal -> Three JSVal
foreign import javascript unsafe "($2).worldToLocal($1)"
    thr_worldToLocal :: JSVal -> JSVal -> Three JSVal

-- | lookAt a position, the vector should be in the world coordinate
foreign import javascript unsafe "($2).lookAt($1)"
    thr_lookAt :: JSVal -> JSVal -> Three ()

-- | rotate on axis (which is normalized)
foreign import javascript unsafe "($3).rotateOnAxis($1, $2)"
    thr_rotateOnAxis :: JSVal -> Double -> JSVal -> Three ()

-- | The global transform of the object. If the Object3d has no parent,
-- then it's identical to the local transform.
foreign import javascript safe "($1).matrixWorld"
    thr_matrixWorld :: JSVal -> JSVal

-- | updatesglobal transform of the object and its children
foreign import javascript unsafe "($1).updateMatrixWorld()"
    thr_updateMatrixWorld :: JSVal -> Three ()

class (ThreeJSVal o) => IsObject3D o where
    getObject3D :: o -> Object3D
    getObject3D = fromJSVal . toJSVal

    -- functions with default implementations
    scale :: o -> TVector
    scale = toTVector . fromJSVal . thr_scale . toJSVal

    setScale :: TVector -> o -> Three ()
    setScale s o = do
        sv <- mkVector s
        thr_setScale (toJSVal sv) (toJSVal o)

    position :: o -> TVector
    position = toTVector . fromJSVal . thr_position . toJSVal

    setPosition :: TVector -> o -> Three ()
    setPosition p o = do
        pv <- mkVector p
        thr_setPosition (toJSVal pv) (toJSVal o)

    rotation :: o -> TEuler
    rotation = toTEuler . fromJSVal . thr_rotation . toJSVal

    setRotation :: TEuler -> o -> Three ()
    setRotation r o = do
        re <- mkEuler r
        thr_setRotation (toJSVal re) (toJSVal o)

    up :: o -> TVector
    up = toTVector . fromJSVal . thr_up . toJSVal

    setUp :: TVector -> o -> Three ()
    setUp u o = do
        uv <- mkVector u
        thr_setUp (toJSVal uv) (toJSVal o)

    setRenderOrder :: Int -> o -> Three ()
    setRenderOrder r o = thr_setRenderOrder r $ toJSVal o

    translateX :: Double -> o -> Three ()
    translateX x o = thr_translateX x $ toJSVal o

    translateY :: Double -> o -> Three ()
    translateY y o = thr_translateY y $ toJSVal o

    translateZ :: Double -> o -> Three ()
    translateZ z o = thr_translateZ z $ toJSVal o

    translateOnAxis :: NormalVector -> Double -> o -> Three ()
    translateOnAxis v d o = thr_translateOnAxis (toJSVal v) d (toJSVal o)

    localToWorld :: TVector -> o -> Three TVector
    localToWorld v o = do
        vv <- mkVector v
        (toTVector . fromJSVal) <$> thr_localToWorld (toJSVal vv) (toJSVal o)

    worldToLocal :: TVector -> o -> Three TVector
    worldToLocal v o = do
        vv <- mkVector v
        (toTVector . fromJSVal) <$> thr_worldToLocal (toJSVal vv) (toJSVal o)

    lookAt :: TVector -> o -> Three ()
    lookAt v o = do
        vv <- mkVector v
        thr_lookAt (toJSVal vv) (toJSVal o)

    rotateOnAxis :: NormalVector -> Double -> o -> Three ()
    rotateOnAxis v d o = thr_rotateOnAxis (toJSVal v) d (toJSVal o)

    matrixWorld :: o -> Matrix
    matrixWorld = fromJSVal . thr_matrixWorld . toJSVal

    updateMatrixWorld :: o -> Three ()
    updateMatrixWorld = thr_updateMatrixWorld . toJSVal

instance IsObject3D Object3D
