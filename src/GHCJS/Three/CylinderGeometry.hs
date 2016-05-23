{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, TemplateHaskell #-}
module GHCJS.Three.CylinderGeometry
    (CylinderGeometry(..), CylinderParam, cpRadiusTop, cpRadiusBot, cpHeight,
    cpRadiusSegments, cpHeightSegments, cpOpenEnded, cpThetaStart, cpThetaLength,
    mkCylinderGeometry
    ) where

import Data.Default
import Control.Lens

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
    _cpRadiusTop      :: Double,
    _cpRadiusBot      :: Double,
    _cpHeight         :: Double,
    _cpRadiusSegments :: Int,
    _cpHeightSegments :: Int,
    _cpOpenEnded      :: Bool,
    _cpThetaStart     :: Double,
    _cpThetaLength    :: Double
}

makeLenses ''CylinderParam

instance Default CylinderParam where
    def = CylinderParam {
            _cpRadiusTop      = 20,
            _cpRadiusBot      = 20,
            _cpHeight         = 100,
            _cpRadiusSegments = 8,
            _cpHeightSegments = 1,
            _cpOpenEnded      = False,
            _cpThetaStart     = 0,
            _cpThetaLength    = 2 * pi
        }

mkCylinderGeometry :: CylinderParam -> Three CylinderGeometry
mkCylinderGeometry p = fromJSVal <$> thr_mkCylinderGeometry top bot h rs hs oe ts tl
    where top = p ^. cpRadiusTop
          bot = p ^. cpRadiusBot
          h   = p ^. cpHeight
          rs  = p ^. cpRadiusSegments
          hs  = p ^. cpHeightSegments
          oe  = if (p ^. cpOpenEnded) then 1 else 0
          ts  = p ^. cpThetaStart
          tl  = p ^. cpThetaLength
