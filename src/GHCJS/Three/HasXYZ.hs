{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.HasXYZ (
    HasX(..), HasY(..), HasZ(..)
    ) where

import GHCJS.Types
import GHCJS.Three.Monad

foreign import javascript safe "($1)['x']"
    objGetX :: JSVal -> Three Double

foreign import javascript unsafe "($2)['x'] = $1"
    objSetX :: Double -> JSVal -> Three ()

foreign import javascript safe "($1)['y']"
    objGetY :: JSVal -> Three Double

foreign import javascript unsafe "($2)['y'] = $1"
    objSetY :: Double -> JSVal -> Three ()

foreign import javascript safe "($1)['z']"
    objGetZ :: JSVal -> Three Double

foreign import javascript unsafe "($2)['z'] = $1"
    objSetZ :: Double -> JSVal -> Three ()

class (ThreeJSVal o) => HasX o where
    getX :: o -> Three Double
    getX = objGetX . toJSVal

    setX :: Double -> o -> Three ()
    setX x o = objSetX x (toJSVal o)

class (ThreeJSVal o) => HasY o where
    getY :: o -> Three Double
    getY = objGetY . toJSVal

    setY :: Double -> o -> Three ()
    setY y o = objSetY y (toJSVal o)

class (ThreeJSVal o) => HasZ o where
    getZ :: o -> Three Double
    getZ = objGetZ . toJSVal

    setZ :: Double -> o -> Three ()
    setZ z o = objSetZ z (toJSVal o)
