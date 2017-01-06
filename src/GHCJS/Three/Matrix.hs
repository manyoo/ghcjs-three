{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Matrix (
    Matrix4(..), mkMatrix4, elements, getInverse, matrixFromArray, transpose, multiplyMatrices, CanApplyMatrix4(..)
) where

import Data.Functor
import GHCJS.Types
import qualified GHCJS.Marshal as M

import GHCJS.Three.Monad
import GHCJS.Three.CanCopy
import Control.Monad

import JavaScript.Array

import Data.Maybe

-- haskell wrapper for the Matrix4 type
newtype Matrix4 = Matrix4 {
    matrixObject :: BaseObject
} deriving ThreeJSVal

instance CanCopy Matrix4

-- create an identity matrix
foreign import javascript unsafe "new window['THREE']['Matrix4']()"
    thr_mkMatrix :: Three JSVal

mkMatrix4 :: Three Matrix4
mkMatrix4 = fromJSVal <$> thr_mkMatrix

foreign import javascript unsafe "($1).elements"
    thr_elements :: JSVal -> Three JSVal

elements :: Matrix4 -> Three [Double]
elements m = (mapM (fmap (fromMaybe 0) . M.fromJSVal) . fromMaybe []) =<< M.fromJSVal =<< thr_elements (toJSVal m)

foreign import javascript unsafe "($2)['getInverse']($1)"
    thr_getInverse :: JSVal -> JSVal -> Three ()

getInverse :: Matrix4 -> Matrix4 -> Three ()
getInverse m1 m2 = thr_getInverse (toJSVal m1) (toJSVal m2)

foreign import javascript unsafe "($2)['fromArray']($1)"
    thr_fromArray :: JSVal -> JSVal -> Three ()

matrixFromArray :: JSArray -> Matrix4 -> Three ()
matrixFromArray arr m = thr_fromArray (jsval arr) (toJSVal m)

foreign import javascript unsafe "($1)['transpose']()"
    thr_transpose :: JSVal -> Three ()

transpose :: Matrix4 -> Three ()
transpose = thr_transpose . toJSVal

foreign import javascript unsafe "($3)['multiplyMatrices']($1, $2)"
    thr_multiplyMatrices :: JSVal -> JSVal -> JSVal -> Three ()

multiplyMatrices :: Matrix4 -> Matrix4 -> Matrix4 -> Three ()
multiplyMatrices m1 m2 m3 = thr_multiplyMatrices (toJSVal m1) (toJSVal m2) (toJSVal m3)

foreign import javascript unsafe "($2)['applyMatrix4']($1)"
    thr_applyMatrix4 :: JSVal -> JSVal -> Three ()

class ThreeJSVal v => CanApplyMatrix4 v where
    applyMatrix4 :: Matrix4 -> v -> Three ()
    applyMatrix4 m v = thr_applyMatrix4 (toJSVal m) (toJSVal v)
