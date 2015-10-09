{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Geometry (
    Geometry(..), mkGeometry,
    IsGeometry(..),
    BoxGeometry(..), mkBoxGeometry
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal

import Data.Maybe (fromMaybe)

import GHCJS.Three.Monad
import GHCJS.Three.Vector hiding (getObject)
import GHCJS.Three.Disposable

newtype Geometry = Geometry {
    geometryObject :: Object
} deriving (ThreeJSRef)

foreign import javascript unsafe "new window.THREE.Geometry()"
    thr_mkGeometry :: Three JSRef

mkGeometry :: Three Geometry
mkGeometry = fromJSRef <$> thr_mkGeometry

-- | get vertices
foreign import javascript safe "($1).vertices"
    thr_vertices :: JSRef -> JSRef

-- | set vertices
foreign import javascript unsafe "($2).vertices = $1"
    thr_setVectices :: JSRef -> JSRef -> Three ()

-- use Marshal.fromJSRef to convert JSRef -> IO (Maybe [JSRef])
-- and Marshal.toJSRef to convert [JSRef] -> IO JSRef
class ThreeJSRef g => IsGeometry g where
    vertices :: g -> Three [Vector]
    vertices g = (map fromJSRef . fromMaybe []) <$> (Marshal.fromJSRef $ thr_vertices $ toJSRef g)

    setVertices :: [Vector] -> g -> Three ()
    setVertices vs g = (Marshal.toJSRef $ map toJSRef vs) >>= flip thr_setVectices (toJSRef g)

instance IsGeometry Geometry
instance Disposable Geometry

-- | BoxGeometry
newtype BoxGeometry = BoxGeometry {
    getGeometry :: Geometry
} deriving (ThreeJSRef, IsGeometry, Disposable)

foreign import javascript unsafe "new window.THREE.BoxGeometry($1, $2, $3)"
    thr_mkBoxGeometry :: Double -> Double -> Double -> Three JSRef

-- | create a new BoxGeometry
mkBoxGeometry :: Double -> Double -> Double -> Three BoxGeometry
mkBoxGeometry w h d = fromJSRef <$> thr_mkBoxGeometry w h d
