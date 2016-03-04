{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Vector (
    TVector(..), TVector2(..), Vector(..), Vector2(..), NormalVector(..),
    mkVector, toTVector, mkVector2, toTVector2, vector3To2
    ) where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.HasXYZ
import GHCJS.Three.Matrix

-- empty class only used for restricting data types below
class ThreeJSVal v => IsJSVector v

-- haskell version of 3D vector
data TVector = TVector {
    x :: Double,
    y :: Double,
    z :: Double
} deriving (Show, Eq)

-- JS version of 3D Vector
newtype Vector = Vector {
    vectorObject :: BaseObject
} deriving (ThreeJSVal)

instance IsJSVector Vector
instance HasX Vector
instance HasY Vector
instance HasZ Vector

foreign import javascript unsafe "($2).setFromMatrixPosition($1)"
    thr_setFromMatrixPosition :: JSVal -> JSVal -> Three ()

fromMatrixPosition :: Matrix -> Three TVector
fromMatrixPosition m = do
    jv <- thr_mkVector 0 0 0
    thr_setFromMatrixPosition (toJSVal m) jv
    return $ toTVector $ fromJSVal jv

-- haskell version of 2D vector
data TVector2 = TVector2 Double Double

-- JS version of 2D vector
newtype Vector2 = Vector2 {
    vector2Object :: BaseObject
} deriving (ThreeJSVal)

instance IsJSVector Vector2
instance HasX Vector2
instance HasY Vector2

-- normal vector is a special type of vector
type NormalVector = Vector

foreign import javascript unsafe "new window.THREE.Vector3($1, $2, $3)"
    thr_mkVector :: Double -> Double -> Double -> Three JSVal

foreign import javascript unsafe "new window.THREE.Vector2($1, $2)"
    thr_mkVector2 :: Double -> Double -> Three JSVal

foreign import javascript safe "($1).x"
    thr_vecX :: JSVal -> Double
foreign import javascript safe "($1).y"
    thr_vecY :: JSVal -> Double
foreign import javascript safe "($1).z"
    thr_vecZ :: JSVal -> Double

vecX :: IsJSVector v => v -> Double
vecX = thr_vecX . toJSVal

vecY :: IsJSVector v => v -> Double
vecY = thr_vecY . toJSVal

vecZ :: IsJSVector v => v -> Double
vecZ = thr_vecZ . toJSVal

-- | create a new Three Vector3 object with TVector
mkVector :: TVector -> Three Vector
mkVector v = fromJSVal <$> thr_mkVector (x v) (y v) (z v)

mkVector2 :: TVector2 -> Three Vector2
mkVector2 (TVector2 x y) = fromJSVal <$> thr_mkVector2 x y

-- | convert Vector to TVector
toTVector :: Vector -> TVector
toTVector v = TVector (vecX v) (vecY v) (vecZ v)

toTVector2 :: Vector2 -> TVector2
toTVector2 v = TVector2 (vecX v) (vecY v)

-- | extract x and y fields of a 3D vector to form a new 2D vector
vector3To2 :: TVector -> TVector2
vector3To2 (TVector x y _) = TVector2 x y
