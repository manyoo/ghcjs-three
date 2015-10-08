{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Object3D where

import GHCJS.Three.Monad
import GHCJS.Three.Vector

data CObject3D a
type Object3D a = Object (CObject3D a)


-- | create a new Object3D object
foreign import javascript unsafe "new window.THREE.Object3D()"
    mkObject3D :: Three (Object3D ())

-- | get scale
foreign import javascript safe "($1).scale"
    scale :: Object3D a -> Vector ()

-- | set scale
foreign import javascript unsafe "($2).scale = $1"
    setScale :: Vector a -> Object3D b -> Three ()

-- | get position
foreign import javascript safe "($1).position"
    position :: Object3D a -> Vector ()

-- | set position
foreign import javascript unsafe "($2).position = $1"
    setPosition :: Vector a -> Object3D b -> Three ()

-- | get rotation
foreign import javascript safe "($1).rotation"
    rotation :: Object3D a -> Vector ()

-- | set rotation
foreign import javascript unsafe "($2).rotation = $1"
    setRotation :: Vector a -> Object3D b -> Three ()

-- | get up direction
foreign import javascript safe "($1).up"
    up :: Object3D a -> Vector ()

-- | set up direction
foreign import javascript unsafe "($2).up = $1"
    setUp :: Vector a -> Object3D b -> Three ()

-- | translate along the X, Y, Z axises
foreign import javascript unsafe "($2).translateX($1)"
    translateX :: Double -> Object3D a -> Three ()
foreign import javascript unsafe "($2).translateY($1)"
    translateY :: Double -> Object3D a -> Three ()
foreign import javascript unsafe "($2).translateZ($1)"
    translateZ :: Double -> Object3D a -> Three ()

-- | translate along an axis, the axis should be a normalized vector
foreign import javascript unsafe "($3).translateOnAxis($1, $2)"
    translateOnAxis :: NormalVector a -> Double -> Object3D b -> Three ()

-- | translate between local and world coordinates
foreign import javascript unsafe "($2).localToWorld($1)"
    localToWorld :: Vector a -> Object3D b -> Three (Vector ())
foreign import javascript unsafe "($2).worldToLocal($1)"
    worldToLocal :: Vector a -> Object3D b -> Three (Vector ())

-- | lookAt a position, the vector should be in the world coordinate
foreign import javascript unsafe "($2).lookAt($1)"
    lookAt :: Vector a -> Object3D b -> Three ()

-- | rotate on axis (which is normalized)
foreign import javascript unsafe "($3).rotateOnAxis($1, $2)"
    rotateOnAxis :: NormalVector a -> Double -> Object3D b -> Three ()
