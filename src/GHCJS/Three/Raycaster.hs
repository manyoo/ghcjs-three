{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Raycaster (
    Raycaster(..), RaycastResult(..),
    mkRaycaster, intersectObject, intersectObjects, getCastPoint, getCastObject
) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal
import Data.Maybe (fromMaybe)
import GHCJS.Three.Monad
import GHCJS.Three.Object3D
import GHCJS.Three.Vector

-- | Raycaster definition
newtype Raycaster = Raycaster {
    getObject :: Object
} deriving (ThreeJSRef)

-- | RaycastResult definition
newtype RaycastResult = RaycastResult {
    rcResObject :: Object
} deriving (ThreeJSRef)

foreign import javascript unsafe "($2).intersectObject($1)"
    thr_intersectObject :: JSRef -> JSRef -> Three JSRef
foreign import javascript unsafe "($2).intersectObjects($1)"
    thr_intersectObjects :: JSRef -> JSRef -> Three JSRef

getResult :: Maybe [JSRef] -> [RaycastResult]
getResult = map fromJSRef . fromMaybe []

-- | intersectObject
intersectObject :: IsObject3D obj => obj -> Raycaster -> Three [RaycastResult]
intersectObject obj ray = getResult <$> (thr_intersectObject (toJSRef obj) (toJSRef ray) >>= Marshal.fromJSRef)

-- | intersectObjects
intersectObjects :: IsObject3D obj => [obj] -> Raycaster -> Three [RaycastResult]
intersectObjects objs ray = getResult <$> ((Marshal.toJSRef $ map toJSRef objs) >>= flip thr_intersectObjects (toJSRef ray) >>= Marshal.fromJSRef)

-- | create a new raycaster
foreign import javascript unsafe "new window.THREE.Raycaster($1, $2, $3, $4)"
    thr_mkRaycaster :: JSRef -> JSRef -> Double -> Double -> Three JSRef

type Near = Double
type Far = Double

mkRaycaster :: Vector -> Vector -> Near -> Far -> Three Raycaster
mkRaycaster origin direction near far = fromJSRef <$> thr_mkRaycaster (toJSRef origin) (toJSRef direction) near far

-- | get raycast point and object from the result
foreign import javascript safe "($1).point"
    thr_point :: JSRef -> JSRef

foreign import javascript safe "($1).object"
    thr_object :: JSRef -> JSRef

getCastPoint :: RaycastResult -> Vector
getCastPoint = fromJSRef <$> thr_point . toJSRef

getCastObject :: RaycastResult -> Object3D
getCastObject = fromJSRef <$> thr_object . toJSRef
