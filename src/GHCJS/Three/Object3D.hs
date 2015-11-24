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
import GHCJS.Three.GLNode
import GHCJS.Three.Visible

newtype Object3D = Object3D {
    getObject :: BaseObject
} deriving (ThreeJSVal)

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
foreign import javascript unsafe "($2).scale = $1"
    thr_setScale :: JSVal -> JSVal -> Three ()

-- | get position
foreign import javascript safe "($1).position"
    thr_position :: JSVal -> JSVal

-- | set position
foreign import javascript unsafe "($2).position = $1"
    thr_setPosition :: JSVal -> JSVal -> Three ()

-- | get rotation
foreign import javascript safe "($1).rotation"
    thr_rotation :: JSVal -> JSVal

-- | set rotation
foreign import javascript unsafe "($2).rotation = $1"
    thr_setRotation :: JSVal -> JSVal -> Three ()

-- | get up direction
foreign import javascript safe "($1).up"
    thr_up :: JSVal -> JSVal

-- | set up direction
foreign import javascript unsafe "($2).up = $1"
    thr_setUp :: JSVal -> JSVal -> Three ()

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


class (ThreeJSVal o) => IsObject3D o where
    -- functions with default implementations
    scale :: o -> Vector
    scale = fromJSVal . thr_scale . toJSVal

    setScale :: Vector -> o -> Three ()
    setScale s o = thr_setScale (toJSVal s) (toJSVal o)

    position :: o -> Vector
    position = fromJSVal . thr_position . toJSVal

    setPosition :: Vector -> o -> Three ()
    setPosition p o = thr_setPosition (toJSVal p) (toJSVal o)

    rotation :: o -> Vector
    rotation = fromJSVal . thr_rotation . toJSVal

    setRotation :: Vector -> o -> Three ()
    setRotation r o = thr_setRotation (toJSVal r) (toJSVal o)

    up :: o -> Vector
    up = fromJSVal . thr_up . toJSVal

    setUp :: Vector -> o -> Three ()
    setUp u o = thr_setUp (toJSVal u) (toJSVal o)

    translateX :: Double -> o -> Three ()
    translateX x o = thr_translateX x $ toJSVal o

    translateY :: Double -> o -> Three ()
    translateY y o = thr_translateY y $ toJSVal o

    translateZ :: Double -> o -> Three ()
    translateZ z o = thr_translateZ z $ toJSVal o

    translateOnAxis :: NormalVector -> Double -> o -> Three ()
    translateOnAxis v d o = thr_translateOnAxis (toJSVal v) d (toJSVal o)

    localToWorld :: Vector -> o -> Three Vector
    localToWorld v o = fromJSVal <$> thr_localToWorld (toJSVal v) (toJSVal o)

    worldToLocal :: Vector -> o -> Three Vector
    worldToLocal v o = fromJSVal <$> thr_worldToLocal (toJSVal v) (toJSVal o)

    lookAt :: Vector -> o -> Three ()
    lookAt v o = thr_lookAt (toJSVal v) (toJSVal o)

    rotateOnAxis :: NormalVector -> Double -> o -> Three ()
    rotateOnAxis v d o = thr_rotateOnAxis (toJSVal v) d (toJSVal o)

instance IsObject3D Object3D
instance IsObject3D o => IsGLNode o
