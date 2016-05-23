{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Euler (
  TEuler(..), Euler(..), mkEuler, toTEuler
  ) where

import Data.JSString
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.HasXYZ

-- haskell version of Euler
data TEuler = TEuler {
  eX     :: Double,
  eY     :: Double,
  eZ     :: Double,
  eOrder :: JSString
  } deriving (Show, Eq)

-- JS version of Euler value
newtype Euler = Euler {
    eulerObject :: BaseObject
} deriving (ThreeJSVal)

instance HasX Euler
instance HasY Euler
instance HasZ Euler

foreign import javascript safe "($1)['x']"
    thr_eulerX :: JSVal -> Double

foreign import javascript safe "($1)['y']"
    thr_eulerY :: JSVal -> Double

foreign import javascript safe "($1)['z']"
    thr_eulerZ :: JSVal -> Double

foreign import javascript safe "($1)['order']"
    thr_order :: JSVal -> JSString

eulerX :: Euler -> Double
eulerX = thr_eulerX . toJSVal

eulerY :: Euler -> Double
eulerY = thr_eulerY . toJSVal

eulerZ :: Euler -> Double
eulerZ = thr_eulerZ . toJSVal

eulerOrder :: Euler -> JSString
eulerOrder = thr_order . toJSVal

foreign import javascript unsafe "new window['THREE']['Euler']($1, $2, $3, $4)"
    thr_mkEuler :: Double -> Double -> Double -> JSString -> Three JSVal

mkEuler :: TEuler -> Three Euler
mkEuler (TEuler x y z o) = fromJSVal <$> thr_mkEuler x y z o

toTEuler :: Euler -> TEuler
toTEuler v = TEuler (eulerX v) (eulerY v) (eulerZ v) (eulerOrder v)
