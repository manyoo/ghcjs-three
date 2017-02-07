{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Raycaster (
    Raycaster(..), RaycastResult(..), setRay, setRayCasterNear, setRayCasterFar, setFromCamera, getRay, mkBaseRaycaster,
    mkRaycaster, intersectObject, intersectingObject, intersectObjects, getCastPoint, getCastObject, getCastFace,
    getCastFaceIndex
) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal
import Control.Monad
import Data.Maybe (fromMaybe)

import GHCJS.Three.Monad
import GHCJS.Three.Object3D
import GHCJS.Three.Face3
import GHCJS.Three.Vector
import GHCJS.Three.Camera
import GHCJS.Three.Ray

-- | Raycaster definition
newtype Raycaster = Raycaster {
    raycasterObject :: BaseObject
} deriving (ThreeJSVal)

-- | RaycastResult definition
newtype RaycastResult = RaycastResult {
    rcResObject :: BaseObject
} deriving (ThreeJSVal)

foreign import javascript unsafe "($2)['intersectObject']($1)"
    thr_intersectObject :: JSVal -> JSVal -> Three JSVal
foreign import javascript unsafe "(($2)['intersectObject']($1)).length"
    thr_intersectingObject :: JSVal -> JSVal -> Three Int
foreign import javascript unsafe "($2)['intersectObjects']($1)"
    thr_intersectObjects :: JSVal -> JSVal -> Three JSVal

getResult :: Maybe [JSVal] -> [RaycastResult]
getResult = map fromJSVal . fromMaybe []

foreign import javascript unsafe "($3)['set']($1, $2)"
    thr_setRay :: JSVal -> JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($7)['ray']['origin']['set']($1, $2, $3);\
                                 \($7)['ray']['direction']['set']($4, $5, $6);"
    thr_setRayValue :: Double -> Double -> Double -> Double -> Double -> Double -> JSVal -> Three ()

setRay :: Vector3 -> Vector3 -> Raycaster -> Three ()
setRay orig dir c = thr_setRayValue (v3x orig) (v3y orig) (v3z orig) (v3x dir) (v3y dir) (v3z dir) (toJSVal c)

foreign import javascript unsafe "($2)['near'] = $1"
    thr_setRayCasterNear :: Double -> JSVal -> Three ()

setRayCasterNear :: Double -> Raycaster -> Three ()
setRayCasterNear n c = thr_setRayCasterNear n (toJSVal c)

foreign import javascript unsafe "($2)['far'] = $1"
    thr_setRayCasterFar :: Double -> JSVal -> Three ()

setRayCasterFar :: Double -> Raycaster -> Three ()
setRayCasterFar f c = thr_setRayCasterFar f (toJSVal c)

foreign import javascript unsafe "($3)['setFromCamera']($1, $2)"
    thr_setFromCamera :: JSVal -> JSVal -> JSVal -> Three ()

setFromCamera :: (IsCamera c) => Vector2 -> c -> Raycaster -> Three ()
setFromCamera v c r = do
    vecVal <- toJSVal <$> mkTVector2 v
    thr_setFromCamera vecVal (toJSVal c) (toJSVal r)

foreign import javascript unsafe "($1)['ray']"
    thr_getRay :: JSVal -> Three JSVal

getRay :: Raycaster -> Three Ray
getRay = fmap fromJSVal . thr_getRay . toJSVal

-- | intersectObject
intersectObject :: IsObject3D obj => obj -> Raycaster -> Three [RaycastResult]
intersectObject obj ray = getResult <$> (thr_intersectObject (toJSVal obj) (toJSVal ray) >>= Marshal.fromJSVal)

intersectingObject :: IsObject3D obj => obj -> Raycaster -> Three Bool
intersectingObject obj ray = (> 0) <$> thr_intersectingObject (toJSVal obj) (toJSVal ray)

-- | intersectObjects
intersectObjects :: IsObject3D obj => [obj] -> Raycaster -> Three [RaycastResult]
intersectObjects objs ray = getResult <$> ((Marshal.toJSVal $ map toJSVal objs) >>= flip thr_intersectObjects (toJSVal ray) >>= Marshal.fromJSVal)

-- | create a new raycaster
foreign import javascript unsafe "new window['THREE']['Raycaster']($1, $2, $3, $4)"
    thr_mkRaycaster :: JSVal -> JSVal -> Double -> Double -> Three JSVal

foreign import javascript unsafe "new window['THREE']['Raycaster']()"
    thr_mkBaseRaycaster :: Three JSVal

mkRaycaster :: Vector3 -> Vector3 -> Near -> Far -> Three Raycaster
mkRaycaster origin direction near far = do
    ov <- mkTVector3 origin
    dv <- mkTVector3 direction
    fromJSVal <$> thr_mkRaycaster (toJSVal ov) (toJSVal dv) near far

mkBaseRaycaster :: Three Raycaster
mkBaseRaycaster = fromJSVal <$> thr_mkBaseRaycaster

-- | get raycast point and object from the result
foreign import javascript unsafe "($1)['point']"
    thr_point :: JSVal -> Three JSVal

foreign import javascript unsafe "($1)['object']"
    thr_object :: JSVal -> Three JSVal

foreign import javascript unsafe "($1)['face']"
    thr_face :: JSVal -> Three JSVal

foreign import javascript unsafe "($1)['faceIndex']"
    thr_faceIndex :: JSVal -> Three Int

getCastPoint :: RaycastResult -> Three Vector3
getCastPoint = (toVector3 . fromJSVal) <=< (thr_point . toJSVal)

getCastObject :: RaycastResult -> Three Object3D
getCastObject = fmap fromJSVal . thr_object . toJSVal

getCastFace :: RaycastResult -> Three Face3
getCastFace = fmap fromJSVal . thr_face . toJSVal

getCastFaceIndex :: RaycastResult -> Three Int
getCastFaceIndex = thr_faceIndex . toJSVal
