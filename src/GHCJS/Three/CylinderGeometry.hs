{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.CylinderGeometry
    (CylinderGeometry(..), CylinderParam, cpRadiusTop, cpRadiusBot, cpHeight,
    cpRadiusSegments, cpHeightSegments, cpOpenEnded, cpThetaStart, cpThetaLength,
    mkCylinderGeometry
    ) where

import Data.Default

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Disposable
import GHCJS.Three.Geometry

newtype CylinderGeometry = CylinderGeometry {
    getCylinderGeometry :: Geometry
} deriving (ThreeJSVal, IsGeometry, Disposable)

foreign import javascript unsafe "new window['THREE']['CylinderGeometry']($1, $2, $3, $4, $5, $6 === 1, $7, $8)"
    thr_mkCylinderGeometry :: Double -> Double -> Double -> Int -> Int -> Int -> Double -> Double -> Three JSVal

-- parameter data type
data CylinderParam = CylinderParam {
    cpRadiusTop      :: Double,
    cpRadiusBot      :: Double,
    cpHeight         :: Double,
    cpRadiusSegments :: Int,
    cpHeightSegments :: Int,
    cpOpenEnded      :: Bool,
    cpThetaStart     :: Double,
    cpThetaLength    :: Double
}

instance Default CylinderParam where
    def = CylinderParam {
            cpRadiusTop      = 20,
            cpRadiusBot      = 20,
            cpHeight         = 100,
            cpRadiusSegments = 8,
            cpHeightSegments = 1,
            cpOpenEnded      = False,
            cpThetaStart     = 0,
            cpThetaLength    = 2 * pi
        }

mkCylinderGeometry :: CylinderParam -> Three CylinderGeometry
mkCylinderGeometry p = fromJSVal <$> thr_mkCylinderGeometry top bot h rs hs oe ts tl
    where top = cpRadiusTop p
          bot = cpRadiusBot p
          h   = cpHeight p
          rs  = cpRadiusSegments p
          hs  = cpHeightSegments p
          oe  = if (cpOpenEnded p) then 1 else 0
          ts  = cpThetaStart p
          tl  = cpThetaLength p
