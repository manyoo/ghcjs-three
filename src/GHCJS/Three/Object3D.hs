{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Object3D (
    Object3D(..),
    mkObject3D,
    IsObject3D(..)
    ) where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Vector hiding (getObject)

newtype Object3D a = Object3D {
    getObject :: Object a
}

instance ThreeJSRef (Object3D a) where
    toJSRef = toJSRef . getObject
    fromJSRef = Object3D . fromJSRef


-- | create a new Object3D object
foreign import javascript unsafe "new window.THREE.Object3D()"
    thr_mkObject3D :: Three JSRef

mkObject3D :: Three (Object3D a)
mkObject3D = fromJSRef <$> thr_mkObject3D

-- | get scale
foreign import javascript safe "($1).scale"
    thr_scale :: JSRef -> JSRef

-- | set scale
foreign import javascript unsafe "($2).scale = $1"
    thr_setScale :: JSRef -> JSRef -> Three ()

-- | get position
foreign import javascript safe "($1).position"
    thr_position :: JSRef -> JSRef

-- | set position
foreign import javascript unsafe "($2).position = $1"
    thr_setPosition :: JSRef -> JSRef -> Three ()

-- | get rotation
foreign import javascript safe "($1).rotation"
    thr_rotation :: JSRef -> JSRef

-- | set rotation
foreign import javascript unsafe "($2).rotation = $1"
    thr_setRotation :: JSRef -> JSRef -> Three ()

-- | get up direction
foreign import javascript safe "($1).up"
    thr_up :: JSRef -> JSRef

-- | set up direction
foreign import javascript unsafe "($2).up = $1"
    thr_setUp :: JSRef -> JSRef -> Three ()

-- | translate along the X, Y, Z axises
foreign import javascript unsafe "($2).translateX($1)"
    thr_translateX :: Double -> JSRef -> Three ()
foreign import javascript unsafe "($2).translateY($1)"
    thr_translateY :: Double -> JSRef -> Three ()
foreign import javascript unsafe "($2).translateZ($1)"
    thr_translateZ :: Double -> JSRef -> Three ()

-- | translate along an axis, the axis should be a normalized vector
foreign import javascript unsafe "($3).translateOnAxis($1, $2)"
    thr_translateOnAxis :: JSRef -> Double -> JSRef -> Three ()

-- | translate between local and world coordinates
foreign import javascript unsafe "($2).localToWorld($1)"
    thr_localToWorld :: JSRef -> JSRef -> Three JSRef
foreign import javascript unsafe "($2).worldToLocal($1)"
    thr_worldToLocal :: JSRef -> JSRef -> Three JSRef

-- | lookAt a position, the vector should be in the world coordinate
foreign import javascript unsafe "($2).lookAt($1)"
    thr_lookAt :: JSRef -> JSRef -> Three ()

-- | rotate on axis (which is normalized)
foreign import javascript unsafe "($3).rotateOnAxis($1, $2)"
    thr_rotateOnAxis :: JSRef -> Double -> JSRef -> Three ()


class (ThreeJSRef o) => IsObject3D o where
    -- functions with default implementations
    scale :: o -> Vector ()
    scale = fromJSRef . thr_scale . toJSRef

    setScale :: Vector a -> o -> Three ()
    setScale s o = thr_setScale (toJSRef s) (toJSRef o)

    position :: o -> Vector ()
    position = fromJSRef . thr_position . toJSRef

    setPosition :: Vector a -> o -> Three ()
    setPosition p o = thr_setPosition (toJSRef p) (toJSRef o)

    rotation :: o -> Vector ()
    rotation = fromJSRef . thr_rotation . toJSRef

    setRotation :: Vector a -> o -> Three ()
    setRotation r o = thr_setRotation (toJSRef r) (toJSRef o)

    up :: o -> Vector ()
    up = fromJSRef . thr_up . toJSRef

    setUp :: Vector a -> o -> Three ()
    setUp u o = thr_setUp (toJSRef u) (toJSRef o)

    translateX :: Double -> o -> Three ()
    translateX x o = thr_translateX x $ toJSRef o

    translateY :: Double -> o -> Three ()
    translateY y o = thr_translateY y $ toJSRef o

    translateZ :: Double -> o -> Three ()
    translateZ z o = thr_translateZ z $ toJSRef o

    translateOnAxis :: NormalVector a -> Double -> o -> Three ()
    translateOnAxis v d o = thr_translateOnAxis (toJSRef v) d (toJSRef o)

    localToWorld :: Vector a -> o -> Three (Vector ())
    localToWorld v o = fromJSRef <$> thr_localToWorld (toJSRef v) (toJSRef o)

    worldToLocal :: Vector a -> o -> Three (Vector ())
    worldToLocal v o = fromJSRef <$> thr_worldToLocal (toJSRef v) (toJSRef o)

    lookAt :: Vector a -> o -> Three ()
    lookAt v o = thr_lookAt (toJSRef v) (toJSRef o)

    rotateOnAxis :: NormalVector a -> Double -> o -> Three ()
    rotateOnAxis v d o = thr_rotateOnAxis (toJSRef v) d (toJSRef o)

instance IsObject3D (Object3D a)
