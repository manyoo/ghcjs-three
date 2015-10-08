{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.HasXYZ (
    HasXYZ(..)
    ) where

import GHCJS.Types
import GHCJS.Three.Monad

foreign import javascript safe "($1).x"
    objGetX :: JSRef -> Double

foreign import javascript unsafe "($2).x = $1"
    objSetX :: Double -> JSRef -> Three ()

foreign import javascript safe "($1).y"
    objGetY :: JSRef -> Double

foreign import javascript unsafe "($2).y = $1"
    objSetY :: Double -> JSRef -> Three ()

foreign import javascript safe "($1).z"
    objGetZ :: JSRef -> Double

foreign import javascript unsafe "($2).z = $1"
    objSetZ :: Double -> JSRef -> Three ()

class (ThreeJSRef o) => HasXYZ o where
    getX :: o -> Double
    getX = objGetX . toJSRef

    setX :: Double -> o -> Three ()
    setX x o = objSetX x (toJSRef o)

    getY :: o -> Double
    getY = objGetY . toJSRef

    setY :: Double -> o -> Three ()
    setY y o = objSetY y (toJSRef o)

    getZ :: o -> Double
    getZ = objGetZ . toJSRef

    setZ :: Double -> o -> Three ()
    setZ z o = objSetZ z (toJSRef o)
