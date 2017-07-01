{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, MultiParamTypeClasses, FlexibleInstances, UndecidableInstances #-}
module GHCJS.Three.Quaternion (
    Quaternion(..),
    mkQuaternion,
    IsQuaternion(..)
    ) where

import Control.Monad
import Data.Functor
import GHCJS.Types
import qualified GHCJS.Marshal as M

import GHCJS.Three.Monad
import GHCJS.Three.Vector
import GHCJS.Three.Matrix
import GHCJS.Three.Euler
import GHCJS.Three.CanCopy
import GHCJS.Three.HasXYZ

-- | Quaternions represent rotation in 3D space
newtype Quaternion = Quaternion {
    getObject :: BaseObject
} deriving (ThreeJSVal)

instance CanCopy Quaternion
instance HasX Quaternion
instance HasY Quaternion
instance HasZ Quaternion
instance HasW Quaternion

foreign import javascript unsafe "new window['THREE']['Quaternion']($1, $2, $3, $4)"
    thr_mkQuaternion :: Double -> Double -> Double -> Double -> Three JSVal

-- | Create a new Quaternion object from x, y, z, and w
mkQuaternion :: Double -> Double -> Double -> Double -> Three Quaternion
mkQuaternion x y z w = fromJSVal <$> thr_mkQuaternion x y z w

class (ThreeJSVal o) => IsQuaternion o where
  -- | Set this quaternion to describe the opposite rotation
  conjugate :: o -> Three ()
  conjugate = thr_conjugate . toJSVal

  -- | Return the dot product of this quaternion with another
  dot :: o -> o -> Three Double
  dot q1 q2 = thr_dot (toJSVal q1) (toJSVal q2)

  -- | Conjugate and normalize a quaternion
  inverse :: o -> Three ()
  inverse = thr_inverse . toJSVal

  -- | The length of a quaternion as a 4-D vector
  qLength :: o -> Three Double
  qLength = thr_qLength . toJSVal

  -- | The length of a quaternion as a 4-D vector, squared
  --
  -- This is useful if you want to compare lengths, as it saves
  -- two square root calculations for the comparison
  qLengthSq :: o -> Three Double
  qLengthSq = thr_qLengthSq . toJSVal

  -- | Normalize a quaternion (so that it has length 1)
  qNormalize :: o -> Three ()
  qNormalize = thr_qNormalize . toJSVal

  -- | Multiply this quaternion by another and put the result in this one
  qMultiply :: o -> o -> Three ()
  qMultiply q o = thr_qMultiply (toJSVal q) (toJSVal o)

  -- | Set this quaternion to be the product of two others
  multiplyQuaternions :: o -> o -> o -> Three ()
  multiplyQuaternions q r o = thr_multiplyQuaternions (toJSVal q) (toJSVal r) (toJSVal o)

  -- | Multiply another quaternion by this one and put the result in this one
  premultiply :: o -> o -> Three ()
  premultiply q o = thr_premultiply (toJSVal q) (toJSVal o)

  -- | Move this quaternion toward the given one by a specified amount (0 to 1)
  slerp :: o -> Double -> o -> Three ()
  slerp q t o = thr_slerp (toJSVal q) t (toJSVal o)

  -- | Set all four elements of the quaternion at once: x y z w
  setQuaternion :: Double -> Double -> Double -> Double -> o -> Three ()
  setQuaternion x y z w o = thr_setQuaternion x y z w (toJSVal o)

  -- | Set this quaternion equivalent to the given rotations around the three axes
  -- (Euler rotation)
  setFromEuler :: TEuler -> o -> Three ()
  setFromEuler e o = do
    re <- mkEuler e
    thr_setFromEuler (toJSVal re) (toJSVal o)

  -- | Set this quaternion equivalent to the rotation part of this matrix
  setFromRotationMatrix :: Matrix4 -> o -> Three ()
  setFromRotationMatrix m o = thr_setFromRotationMatrix (toJSVal m) (toJSVal o)

  -- | Set this quaternion so that it rotates one vector to the other
  setFromUnitVectors :: NormalVector -> NormalVector -> o -> Three ()
  setFromUnitVectors v1 v2 o = thr_setFromUnitVectors (toJSVal v1) (toJSVal v2) (toJSVal o)

instance IsQuaternion Quaternion

foreign import javascript unsafe "($1)['conjugate']()"
  thr_conjugate :: JSVal -> Three ()

foreign import javascript unsafe "($2)['dot']($1)"
  thr_dot :: JSVal -> JSVal -> Three Double

foreign import javascript unsafe "($1)['inverse']()" 
  thr_inverse :: JSVal -> Three ()

foreign import javascript unsafe "($1)['length']()"
  thr_qLength :: JSVal -> Three Double

foreign import javascript unsafe "($1)['lengthSq']()"
  thr_qLengthSq :: JSVal -> Three Double

foreign import javascript unsafe "($1)['normalize']()"
  thr_qNormalize :: JSVal -> Three ()

foreign import javascript unsafe "($2)['multiply']($1)"
  thr_qMultiply :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($3)['multiplyQuaternions']($1, $2)"
  thr_multiplyQuaternions :: JSVal -> JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['premultiply']($1)"
  thr_premultiply :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($3)['slerp']($1, $2)"
  thr_slerp :: JSVal -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($5)['set']($1, $2, $3, $4)"
  thr_setQuaternion :: Double -> Double -> Double -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($3)['setFromAxisAngle']($1, $2)"
  thr_setFromAxisAngle :: JSVal -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($2)['setFromEuler']($1)"
  thr_setFromEuler :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2)['setFromRotationMatrix']($1)"
  thr_setFromRotationMatrix :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($3)['setFromUnitVectors']($1, $2)"
  thr_setFromUnitVectors :: JSVal -> JSVal -> JSVal -> Three ()

