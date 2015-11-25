{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Euler (
    Euler(..), mkEuler
    ) where

import Data.JSString
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.HasXYZ

newtype Euler = Euler {
    eulerObject :: BaseObject
} deriving (ThreeJSVal)

instance HasX Euler
instance HasY Euler
instance HasZ Euler

foreign import javascript unsafe "new window.THREE.Euler($1, $2, $3, $4)"
    thr_mkEuler :: Double -> Double -> Double -> JSString -> Three JSVal

mkEuler :: Double -> Double -> Double -> JSString -> Three Euler
mkEuler x y z o = fromJSVal <$> thr_mkEuler x y z o
