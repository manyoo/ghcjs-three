{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Geometry (
    Geometry(..), mkGeometry,
    IsGeometry(..), HasBounding(..),
    BoxGeometry(..), mkBoxGeometry,
    CircleGeometry(..), mkCircleGeometry,
    SphereGeometry(..), mkSphereGeometry,
    Radius, WidthSegments, HeightSegments, PhiStart, PhiLength, ThetaStart, ThetaLength
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal

import Data.Maybe (fromMaybe)

import GHCJS.Three.Monad
import GHCJS.Three.Vector hiding (getObject)
import GHCJS.Three.HasName
import GHCJS.Three.Disposable
import GHCJS.Three.Face3
import GHCJS.Three.Box3
import GHCJS.Three.Sphere

newtype Geometry = Geometry {
    geometryObject :: BaseObject
} deriving (ThreeJSVal)

instance HasName Geometry

foreign import javascript unsafe "new window['THREE']['Geometry']()"
    thr_mkGeometry :: Three JSVal

mkGeometry :: Three Geometry
mkGeometry = fromJSVal <$> thr_mkGeometry

-- | get vertices
foreign import javascript unsafe "($1)['vertices']"
    thr_vertices :: JSVal -> Three JSVal

-- | set vertices
foreign import javascript unsafe "($2)['vertices'] = $1"
    thr_setVectices :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['verticesNeedUpdate'] = $1 === 1"
    thr_setVerticesNeedUpdate :: Int -> JSVal -> Three ()

-- | get faces
foreign import javascript unsafe "($1)['faces']"
    thr_faces :: JSVal -> Three JSVal

-- | set vertices
foreign import javascript unsafe "($2)['faces'] = $1"
    thr_setFaces :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['elementsNeedUpdate'] = $1 === 1"
    thr_setElementsNeedUpdate :: Int -> JSVal -> Three ()

foreign import javascript unsafe "($1)['isBufferGeometry']"
    thr_isBufferGeometry :: JSVal -> Three Bool

foreign import javascript unsafe "($1)['computeBoundingBox']()"
    thr_computeBoundingBox :: JSVal -> Three ()

foreign import javascript unsafe "($1)['boundingBox']"
    thr_boundingBox :: JSVal -> Three JSVal

foreign import javascript unsafe "($1)['computeBoundingSphere']()"
    thr_computeBoundingSphere :: JSVal -> Three ()

foreign import javascript unsafe "($1)['boundingSphere']"
    thr_boundingSphere :: JSVal -> Three JSVal

-- use Marshal.fromJSVal to convert JSVal -> IO (Maybe [JSVal])
-- and Marshal.toJSVal to convert [JSVal] -> IO JSVal
class ThreeJSVal g => IsGeometry g where
    vertices :: g -> Three [Vector3]
    vertices g = do
        vs <- thr_vertices (toJSVal g)
        vl <- Marshal.fromJSVal vs
        mapM (toVector3 . fromJSVal) $ fromMaybe [] vl

    setVertices :: [Vector3] -> g -> Three ()
    setVertices vs g = mapM mkTVector3 vs >>= Marshal.toJSVal . map toJSVal >>= flip thr_setVectices (toJSVal g) >> thr_setVerticesNeedUpdate 1 (toJSVal g)

    faces :: g -> Three [Face3]
    faces g = (map fromJSVal . fromMaybe []) <$> (Marshal.fromJSVal =<< thr_vertices (toJSVal g))

    setFaces :: [Face3] -> g -> Three ()
    setFaces fs g = Marshal.toJSVal (map toJSVal fs) >>= flip thr_setFaces (toJSVal g) >> thr_setElementsNeedUpdate 1 (toJSVal g)

    isBufferGeometry :: g -> Three Bool
    isBufferGeometry = thr_isBufferGeometry . toJSVal

class ThreeJSVal g => HasBounding g where
    computeBoundingBox :: g -> Three ()
    computeBoundingBox = thr_computeBoundingBox . toJSVal

    boundingBox :: g -> Three (Maybe Box3)
    boundingBox g = fmap fromJSVal <$> (Marshal.fromJSVal =<< thr_boundingBox (toJSVal g))

    computeBoundingSphere :: g -> Three ()
    computeBoundingSphere = thr_computeBoundingSphere . toJSVal

    boundingSphere :: g -> Three (Maybe Sphere)
    boundingSphere g = fmap fromJSVal <$> (Marshal.fromJSVal =<< thr_boundingSphere (toJSVal g))

instance IsGeometry Geometry
instance HasBounding Geometry
instance Disposable Geometry

-- | BoxGeometry
newtype BoxGeometry = BoxGeometry {
    getGeometry :: Geometry
} deriving (ThreeJSVal, IsGeometry, HasBounding, Disposable)

foreign import javascript unsafe "new window['THREE']['BoxGeometry']($1, $2, $3)"
    thr_mkBoxGeometry :: Double -> Double -> Double -> Three JSVal

-- | create a new BoxGeometry
mkBoxGeometry :: Double -> Double -> Double -> Three BoxGeometry
mkBoxGeometry w h d = fromJSVal <$> thr_mkBoxGeometry w h d

-- | CircleGeometry
newtype CircleGeometry = CircleGeometry {
    getCircleGeometry :: Geometry
} deriving (ThreeJSVal, IsGeometry, HasBounding, Disposable)

foreign import javascript unsafe "new window['THREE']['CircleGeometry']($1, $2)"
    thr_mkCircleGeometry :: Double -> Int -> Three JSVal

-- | create a new CircleGeometry
mkCircleGeometry :: Double -> Int -> Three CircleGeometry
mkCircleGeometry radius segments = fromJSVal <$> thr_mkCircleGeometry radius segments


-- | SphereGeometry
newtype SphereGeometry = SphereGeometry {
    getSphereGeometry :: Geometry
    } deriving (ThreeJSVal, IsGeometry, HasBounding, Disposable)

foreign import javascript unsafe "new window['THREE']['SphereGeometry']($1, $2, $3, $4, $5, $6, $7)"
    thr_mkSphereGeometry :: Double -> Int -> Int -> Double -> Double -> Double -> Double -> Three JSVal

type Radius         = Double
type WidthSegments  = Int
type HeightSegments = Int
type PhiStart       = Double
type PhiLength      = Double
type ThetaStart     = Double
type ThetaLength    = Double

mkSphereGeometry :: Radius -> WidthSegments -> HeightSegments -> PhiStart -> PhiLength -> ThetaStart -> ThetaLength -> Three SphereGeometry
mkSphereGeometry r ws hs ps pl ts tl = fromJSVal <$> thr_mkSphereGeometry r ws hs ps pl ts tl
