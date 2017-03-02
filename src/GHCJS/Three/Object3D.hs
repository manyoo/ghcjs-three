{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module GHCJS.Three.Object3D (
    Object3D(..),
    mkObject3D,
    IsObject3D(..)
    ) where

import Control.Monad
import Data.Functor
import GHCJS.Types
import qualified GHCJS.Marshal as M

import GHCJS.Three.Monad
import GHCJS.Three.Vector
import GHCJS.Three.Matrix
import GHCJS.Three.Euler
import GHCJS.Three.GLNode
import GHCJS.Three.Visible
import GHCJS.Three.HasName
import GHCJS.Three.CanCopy

newtype Object3D = Object3D {
    getObject :: BaseObject
} deriving (ThreeJSVal)

instance IsGLNode Object3D
instance Visible Object3D
instance HasName Object3D
instance CanCopy Object3D

-- | create a new Object3D object
foreign import javascript unsafe "new window['THREE']['Object3D']()"
    thr_mkObject3D :: Three JSVal

mkObject3D :: Three Object3D
mkObject3D = fromJSVal <$> thr_mkObject3D

-- | get children objects
foreign import javascript unsafe "($1)['children']"
    thr_children :: JSVal -> Three JSVal

-- | get scale
foreign import javascript unsafe "($1)['scale']"
    thr_scale :: JSVal -> Three JSVal

-- | set scale
foreign import javascript unsafe "($2)['scale']['copy']($1)"
    thr_setScale :: JSVal -> JSVal -> Three ()

-- | get position
foreign import javascript unsafe "($1)['position']"
    thr_position :: JSVal -> Three JSVal

-- | set position
foreign import javascript unsafe "($2)['position']['copy']($1)"
    thr_setPosition :: JSVal -> JSVal -> Three ()

-- | get rotation
foreign import javascript unsafe "($1)['rotation']"
    thr_rotation :: JSVal -> Three JSVal

-- | set rotation
foreign import javascript unsafe "($2)['rotation']['copy']($1)"
    thr_setRotation :: JSVal -> JSVal -> Three ()

-- | get up direction
foreign import javascript unsafe "($1)['up']"
    thr_up :: JSVal -> Three JSVal

-- | set up direction
foreign import javascript unsafe "($2)['up']['copy']($1)"
    thr_setUp :: JSVal -> JSVal -> Three ()

-- | set renderOrder
foreign import javascript unsafe "($2)['renderOrder'] = $1"
    thr_setRenderOrder :: Int -> JSVal -> Three ()

-- | translate along the X, Y, Z axises
foreign import javascript unsafe "($2)['translateX']($1)"
    thr_translateX :: Double -> JSVal -> Three ()
foreign import javascript unsafe "($2)['translateY']($1)"
    thr_translateY :: Double -> JSVal -> Three ()
foreign import javascript unsafe "($2)['translateZ']($1)"
    thr_translateZ :: Double -> JSVal -> Three ()

-- | translate along an axis, the axis should be a normalized vector
foreign import javascript unsafe "($3)['translateOnAxis']($1, $2)"
    thr_translateOnAxis :: JSVal -> Double -> JSVal -> Three ()

-- | translate between local and world coordinates
foreign import javascript unsafe "($2)['localToWorld']($1)"
    thr_localToWorld :: JSVal -> JSVal -> Three JSVal
foreign import javascript unsafe "($2)['worldToLocal']($1)"
    thr_worldToLocal :: JSVal -> JSVal -> Three JSVal

-- | lookAt a position, the vector should be in the world coordinate
foreign import javascript unsafe "($2)['lookAt']($1)"
    thr_lookAt :: JSVal -> JSVal -> Three ()

-- | rotate around x axis in local space
foreign import javascript unsafe "($2)['rotateX']($1)"
    thr_rotateX :: Double -> JSVal -> Three ()

foreign import javascript unsafe "($2)['rotateY']($1)"
    thr_rotateY :: Double -> JSVal -> Three ()

foreign import javascript unsafe "($2)['rotateZ']($1)"
    thr_rotateZ :: Double -> JSVal -> Three ()

-- | rotate on axis (which is normalized)
foreign import javascript unsafe "($3)['rotateOnAxis']($1, $2)"
    thr_rotateOnAxis :: JSVal -> Double -> JSVal -> Three ()

-- | Gets rendered into shadow map.
foreign import javascript unsafe "($2)['castShadow'] = $1 === 1"
    thr_setCastShadow :: Int -> JSVal -> Three ()

-- | Material gets baked in shadow receiving
foreign import javascript unsafe "($2)['receiveShadow'] = $1 === 1"
    thr_setReceiveShadow :: Int -> JSVal -> Three ()

-- | The global transform of the object. If the Object3d has no parent,
-- then it's identical to the local transform.
foreign import javascript unsafe "($1)['matrixWorld']"
    thr_matrixWorld :: JSVal -> Three JSVal

foreign import javascript unsafe "($2)['matrix']['copy']($1)"
    thr_setMatrix :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['matrixAutoUpdate'] = $1"
    thr_setMatrixAutoUpdate :: Bool -> JSVal -> Three ()

-- | updatesglobal transform of the object and its children
foreign import javascript unsafe "($1)['updateMatrixWorld']()"
    thr_updateMatrixWorld :: JSVal -> Three ()

foreign import javascript unsafe "($2)['modelViewMatrix']['copy']($1)"
    thr_setModelViewMatrix :: JSVal -> JSVal -> Three ()

class (ThreeJSVal o) => IsObject3D o where
    getObject3D :: o -> Object3D
    getObject3D = fromJSVal . toJSVal

    getChildren :: o -> Three [Object3D]
    getChildren o = (fmap fromJSVal . maybeToList) <$> (M.fromJSVal =<< thr_children (toJSVal o))
        where maybeToList Nothing = []
              maybeToList (Just l) = l

    -- functions with default implementations
    scale :: o -> Three Vector3
    scale = (toVector3 . fromJSVal) <=< (thr_scale . toJSVal)

    setScale :: Vector3 -> o -> Three ()
    setScale s o = do
        sv <- mkTVector3 s
        thr_setScale (toJSVal sv) (toJSVal o)

    position :: o -> Three Vector3
    position = (toVector3 . fromJSVal) <=< (thr_position . toJSVal)

    setPosition :: Vector3 -> o -> Three ()
    setPosition p o = do
        pv <- mkTVector3 p
        thr_setPosition (toJSVal pv) (toJSVal o)

    setPositionOrig :: TVector3 -> o -> Three ()
    setPositionOrig p o = thr_setPosition (toJSVal p) (toJSVal o)

    rotation :: o -> Three TEuler
    rotation = (toTEuler . fromJSVal) <=< (thr_rotation . toJSVal)

    setRotation :: TEuler -> o -> Three ()
    setRotation r o = do
        re <- mkEuler r
        thr_setRotation (toJSVal re) (toJSVal o)

    setRotationOrig :: Euler -> o -> Three ()
    setRotationOrig r o = thr_setRotation (toJSVal r) (toJSVal o)

    up :: o -> Three Vector3
    up = (toVector3 . fromJSVal) <=< (thr_up . toJSVal)

    setUp :: Vector3 -> o -> Three ()
    setUp u o = do
        uv <- mkTVector3 u
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

    localToWorld :: Vector3 -> o -> Three Vector3
    localToWorld v o = do
        vv <- mkTVector3 v
        (toVector3 . fromJSVal) =<< thr_localToWorld (toJSVal vv) (toJSVal o)

    worldToLocal :: Vector3 -> o -> Three Vector3
    worldToLocal v o = do
        vv <- mkTVector3 v
        (toVector3 . fromJSVal) =<< thr_worldToLocal (toJSVal vv) (toJSVal o)

    lookAt :: Vector3 -> o -> Three ()
    lookAt v o = do
        vv <- mkTVector3 v
        thr_lookAt (toJSVal vv) (toJSVal o)

    rotateX :: Double -> o -> Three ()
    rotateX x o = thr_rotateX x $ toJSVal o

    rotateY :: Double -> o -> Three ()
    rotateY y o = thr_rotateY y $ toJSVal o

    rotateZ :: Double -> o -> Three ()
    rotateZ z o = thr_rotateZ z $ toJSVal o

    rotateOnAxis :: NormalVector -> Double -> o -> Three ()
    rotateOnAxis v d o = thr_rotateOnAxis (toJSVal v) d (toJSVal o)

    matrixWorld :: o -> Three Matrix4
    matrixWorld = fmap fromJSVal . thr_matrixWorld . toJSVal

    setMatrix :: Matrix4 -> o -> Three ()
    setMatrix m o = thr_setMatrix (toJSVal m) (toJSVal o)

    setMatrixAutoUpdate :: Bool -> o -> Three ()
    setMatrixAutoUpdate u o = thr_setMatrixAutoUpdate u (toJSVal o)

    updateMatrixWorld :: o -> Three ()
    updateMatrixWorld = thr_updateMatrixWorld . toJSVal

    setModelViewMatrix :: Matrix4 -> o -> Three ()
    setModelViewMatrix m o = thr_setModelViewMatrix (toJSVal m) (toJSVal o)

    setCastShadow :: Bool -> o -> Three ()
    setCastShadow b o = thr_setCastShadow (if b then 1 else 0) (toJSVal o)

    setReceiveShadow :: Bool -> o -> Three ()
    setReceiveShadow b o = thr_setReceiveShadow (if b then 1 else 0) (toJSVal o)

instance IsObject3D Object3D
