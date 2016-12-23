{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Ray (
    Ray(..), mkRay, origin, direction, intersectsSphere, intersectsBox, intersectsTriangle
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

foreign import javascript unsafe "new window['THREE']['Ray']()"
    thr_mkRay :: Three JSVal

mkRay :: Three Ray
mkRay = fromJSVal <$> thr_mkRay

foreign import javascript unsafe "($1)['origin']"
    thr_origin :: JSVal -> Three JSVal

origin :: Ray -> Three TVector3
origin = fmap fromJSVal . thr_origin . toJSVal

foreign import javascript unsafe "($1)['direction']"
    thr_direction :: JSVal -> Three JSVal

direction :: Ray -> Three TVector3
direction = fmap fromJSVal . thr_direction . toJSVal

foreign import javascript unsafe "($2)['intersectsSphere']($1)"
    thr_intersectsSphere :: JSVal -> JSVal -> Three Bool

intersectsSphere :: Sphere -> Ray -> Three Bool
intersectsSphere s r = thr_intersectsSphere (toJSVal s) (toJSVal r)

foreign import javascript unsafe "($2)['intersectsBox']($1)"
    thr_intersectsBox :: JSVal -> JSVal -> Three Bool

intersectsBox :: Box3 -> Ray -> Three Bool
intersectsBox b r = thr_intersectsBox (toJSVal b) (toJSVal r)

foreign import javascript unsafe "($5)['intersectTriangle']($1, $2, $3, $4)"
    thr_intersectsTriangle :: JSVal -> JSVal -> JSVal -> Bool -> JSVal -> Three JSVal

intersectsTriangle :: TVector3 -> TVector3 -> TVector3 -> Bool -> Ray -> Three Bool
intersectsTriangle a b c backfaceCulling r = (not . isNull) <$> thr_intersectsTriangle (toJSVal a) (toJSVal b) (toJSVal c) backfaceCulling (toJSVal r)
