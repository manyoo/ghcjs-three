{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Vector (
    TVector(..), Vector(..), NormalVector(..),
    mkVector, toTVector
    ) where

import GHCJS.Three.Monad

data TVector = TVector {
    x :: Double,
    y :: Double,
    z :: Double
} deriving (Show, Eq)

data CVector a
type Vector a = Object (CVector a)

-- normal vector is a special type of vector
data CNormalVector a
type NormalVector a = Vector (CNormalVector a)

foreign import javascript unsafe "new window.THREE.Vector3($1, $2, $3)"
    thr_mkVector :: Double -> Double -> Double -> Three (Vector ())

foreign import javascript safe "($1).x"
    vecX :: Vector a -> Double
foreign import javascript safe "($1).y"
    vecY :: Vector a -> Double
foreign import javascript safe "($1).z"
    vecZ :: Vector a -> Double

-- | create a new Three Vector3 object with TVector
mkVector :: TVector -> Three (Vector ())
mkVector v = thr_mkVector (x v) (y v) (z v)

-- | convert Vector to TVector
toTVector :: Vector a -> TVector
toTVector v = TVector (vecX v) (vecY v) (vecZ v)
