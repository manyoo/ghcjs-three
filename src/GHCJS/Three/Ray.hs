{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Ray (
    Ray(..), origin, direction, intersectsSphere, intersectsBox, intersectsTriangle
    ) where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.CanCopy
import GHCJS.Three.Vector
import GHCJS.Three.Matrix
import GHCJS.Three.Sphere
import GHCJS.Three.Box3

newtype Ray = Ray {
    rayObject :: BaseObject
    } deriving ThreeJSVal

instance CanCopy Ray
instance CanApplyMatrix4 Ray

foreign import javascript unsafe "($1)['origin']"
    thr_origin :: JSVal -> JSVal

origin :: Ray -> TVector3
origin = fromJSVal . thr_origin . toJSVal

foreign import javascript unsafe "($1)['direction']"
    thr_direction :: JSVal -> JSVal

direction :: Ray -> TVector3
direction = fromJSVal . thr_direction . toJSVal

foreign import javascript unsafe "($2)['intersectsSphere']($1)"
    thr_intersectsSphere :: JSVal -> JSVal -> Three Bool

intersectsSphere :: Sphere -> Ray -> Three Bool
intersectsSphere s r = thr_intersectsSphere (toJSVal s) (toJSVal r)

foreign import javascript unsafe "($2)['intersectsBox']($1)"
    thr_intersectsBox :: JSVal -> JSVal -> Three Bool

intersectsBox :: Box3 -> Ray -> Three Bool
intersectsBox b r = thr_intersectsBox (toJSVal b) (toJSVal r)

foreign import javascript unsafe "($5)['intersectTriangle']($1, $2, $3, $4)"
    thr_intersectsTriangle :: Double -> Double -> Double -> Bool -> JSVal -> Three JSVal

intersectsTriangle :: Double -> Double -> Double -> Bool -> Ray -> Three Bool
intersectsTriangle a b c backfaceCulling r = (not . isNull) <$> thr_intersectsTriangle a b c backfaceCulling (toJSVal r)
