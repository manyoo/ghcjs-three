{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Matrix (
    Matrix(..), mkMatrix
) where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad

-- haskell wrapper for the Matrix4 type
newtype Matrix = Matrix {
    matrixObject :: BaseObject
} deriving ThreeJSVal

-- create an identity matrix
foreign import javascript unsafe "new window.THREE.Matrix4()"
    thr_mkMatrix :: Three JSVal

mkMatrix :: Three Matrix
mkMatrix = fromJSVal <$> thr_mkMatrix
