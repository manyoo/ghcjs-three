{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Matrix (
    Matrix(..), mkMatrix, elements
) where

import Data.Functor
import GHCJS.Types
import qualified GHCJS.Marshal as M

import GHCJS.Three.Monad
import Control.Monad

import Data.Maybe

-- haskell wrapper for the Matrix4 type
newtype Matrix = Matrix {
    matrixObject :: BaseObject
} deriving ThreeJSVal

-- create an identity matrix
foreign import javascript unsafe "new window['THREE']['Matrix4']()"
    thr_mkMatrix :: Three JSVal

mkMatrix :: Three Matrix
mkMatrix = fromJSVal <$> thr_mkMatrix

foreign import javascript unsafe "($1).elements"
    thr_elements :: JSVal -> JSVal

elements :: Matrix -> Three [Double]
elements m = (mapM (fmap (fromMaybe 0) . M.fromJSVal) . fromMaybe []) =<< M.fromJSVal (thr_elements $ toJSVal m)
