{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Vector (
    TVector(..), Vector(..), NormalVector(..),
    mkVector, toTVector
    ) where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.HasXYZ

data TVector = TVector {
    x :: Double,
    y :: Double,
    z :: Double
} deriving (Show, Eq)

newtype Vector = Vector {
    vectorObject :: Object
} deriving (ThreeJSVal)

instance HasXYZ Vector

-- normal vector is a special type of vector
type NormalVector = Vector

foreign import javascript unsafe "new window.THREE.Vector3($1, $2, $3)"
    thr_mkVector :: Double -> Double -> Double -> Three JSVal

foreign import javascript safe "($1).x"
    thr_vecX :: JSVal -> Double
foreign import javascript safe "($1).y"
    thr_vecY :: JSVal -> Double
foreign import javascript safe "($1).z"
    thr_vecZ :: JSVal -> Double

vecX :: Vector -> Double
vecX = thr_vecX . toJSVal

vecY :: Vector -> Double
vecY = thr_vecY . toJSVal

vecZ :: Vector -> Double
vecZ = thr_vecZ . toJSVal

-- | create a new Three Vector3 object with TVector
mkVector :: TVector -> Three Vector
mkVector v = fromJSVal <$> thr_mkVector (x v) (y v) (z v)

-- | convert Vector to TVector
toTVector :: Vector -> TVector
toTVector v = TVector (vecX v) (vecY v) (vecZ v)
