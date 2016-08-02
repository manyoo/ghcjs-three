{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Geometry (
    Geometry(..), mkGeometry,
    IsGeometry(..),
    BoxGeometry(..), mkBoxGeometry,
    CircleGeometry(..), mkCircleGeometry
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal

import Data.Maybe (fromMaybe)

import GHCJS.Three.Monad
import GHCJS.Three.Vector hiding (getObject)
import GHCJS.Three.Disposable
import GHCJS.Three.Face3
import Data.JSString (pack, unpack)

newtype Geometry = Geometry {
    geometryObject :: BaseObject
} deriving (ThreeJSVal)

foreign import javascript unsafe "new window['THREE']['Geometry']()"
    thr_mkGeometry :: Three JSVal

mkGeometry :: Three Geometry
mkGeometry = fromJSVal <$> thr_mkGeometry

-- | get vertices
foreign import javascript safe "($1)['vertices']"
    thr_vertices :: JSVal -> JSVal

-- | set vertices
foreign import javascript unsafe "($2)['vertices'] = $1"
    thr_setVectices :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['verticesNeedUpdate'] = $1 === 1"
    thr_setVerticesNeedUpdate :: Int -> JSVal -> Three ()

-- | get faces
foreign import javascript safe "($1)['faces']"
    thr_faces :: JSVal -> JSVal

-- | set vertices
foreign import javascript unsafe "($2)['faces'] = $1"
    thr_setFaces :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['elementsNeedUpdate'] = $1 === 1"
    thr_setElementsNeedUpdate :: Int -> JSVal -> Three ()

-- | get name
foreign import javascript safe "($1)['name']"
    thr_getName :: JSVal -> JSString

-- | set name
foreign import javascript unsafe "($2)['name'] = $1"
    thr_setName :: JSString -> JSVal -> Three ()

-- use Marshal.fromJSVal to convert JSVal -> IO (Maybe [JSVal])
-- and Marshal.toJSVal to convert [JSVal] -> IO JSVal
class ThreeJSVal g => IsGeometry g where
    vertices :: g -> Three [Vector3]
    vertices g = (map (toVector3 . fromJSVal) . fromMaybe []) <$> (Marshal.fromJSVal $ thr_vertices $ toJSVal g)

    setVertices :: [Vector3] -> g -> Three ()
    setVertices vs g = mapM mkTVector3 vs >>= Marshal.toJSVal . map toJSVal >>= flip thr_setVectices (toJSVal g) >> thr_setVerticesNeedUpdate 1 (toJSVal g)

    faces :: g -> Three [Face3]
    faces g = (map fromJSVal . fromMaybe []) <$> (Marshal.fromJSVal $ thr_vertices $ toJSVal g)

    setFaces :: [Face3] -> g -> Three ()
    setFaces fs g = Marshal.toJSVal (map toJSVal fs) >>= flip thr_setFaces (toJSVal g) >> thr_setElementsNeedUpdate 1 (toJSVal g)

    getName :: g -> String
    getName g = unpack $ thr_getName (toJSVal g)

    setName :: String -> g -> Three ()
    setName n g = thr_setName (pack n) (toJSVal g)

instance IsGeometry Geometry
instance Disposable Geometry

-- | BoxGeometry
newtype BoxGeometry = BoxGeometry {
    getGeometry :: Geometry
} deriving (ThreeJSVal, IsGeometry, Disposable)

foreign import javascript unsafe "new window['THREE']['BoxGeometry']($1, $2, $3)"
    thr_mkBoxGeometry :: Double -> Double -> Double -> Three JSVal

-- | create a new BoxGeometry
mkBoxGeometry :: Double -> Double -> Double -> Three BoxGeometry
mkBoxGeometry w h d = fromJSVal <$> thr_mkBoxGeometry w h d

-- | CircleGeometry
newtype CircleGeometry = CircleGeometry {
    getCircleGeometry :: Geometry
} deriving (ThreeJSVal, IsGeometry, Disposable)

foreign import javascript unsafe "new window['THREE']['CircleGeometry']($1, $2)"
    thr_mkCircleGeometry :: Double -> Int -> Three JSVal

-- | create a new CircleGeometry
mkCircleGeometry :: Double -> Int -> Three CircleGeometry
mkCircleGeometry radius segments = fromJSVal <$> thr_mkCircleGeometry radius segments
