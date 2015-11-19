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
    raycasterObject :: BaseObject
} deriving (ThreeJSVal)

-- | RaycastResult definition
newtype RaycastResult = RaycastResult {
    rcResObject :: BaseObject
} deriving (ThreeJSVal)

foreign import javascript unsafe "($2).intersectObject($1)"
    thr_intersectObject :: JSVal -> JSVal -> Three JSVal
foreign import javascript unsafe "($2).intersectObjects($1)"
    thr_intersectObjects :: JSVal -> JSVal -> Three JSVal

getResult :: Maybe [JSVal] -> [RaycastResult]
getResult = map fromJSVal . fromMaybe []

-- | intersectObject
intersectObject :: IsObject3D obj => obj -> Raycaster -> Three [RaycastResult]
intersectObject obj ray = getResult <$> (thr_intersectObject (toJSVal obj) (toJSVal ray) >>= Marshal.fromJSVal)

-- | intersectObjects
intersectObjects :: IsObject3D obj => [obj] -> Raycaster -> Three [RaycastResult]
intersectObjects objs ray = getResult <$> ((Marshal.toJSVal $ map toJSVal objs) >>= flip thr_intersectObjects (toJSVal ray) >>= Marshal.fromJSVal)

-- | create a new raycaster
foreign import javascript unsafe "new window.THREE.Raycaster($1, $2, $3, $4)"
    thr_mkRaycaster :: JSVal -> JSVal -> Double -> Double -> Three JSVal

type Near = Double
type Far = Double

mkRaycaster :: Vector -> Vector -> Near -> Far -> Three Raycaster
mkRaycaster origin direction near far = fromJSVal <$> thr_mkRaycaster (toJSVal origin) (toJSVal direction) near far

-- | get raycast point and object from the result
foreign import javascript safe "($1).point"
    thr_point :: JSVal -> JSVal

foreign import javascript safe "($1).object"
    thr_object :: JSVal -> JSVal

getCastPoint :: RaycastResult -> Vector
getCastPoint = fromJSVal <$> thr_point . toJSVal

getCastObject :: RaycastResult -> Object3D
getCastObject = fromJSVal <$> thr_object . toJSVal
