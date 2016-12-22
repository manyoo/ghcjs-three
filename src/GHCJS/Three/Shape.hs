{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Shape
    (Shape(..), mkShape, holes, setHoles
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal

import Data.Maybe (fromMaybe)

import GHCJS.Three.Monad
import GHCJS.Three.Path

newtype Shape = Shape {
    shapePath :: Path
} deriving (ThreeJSVal, IsPath)

foreign import javascript unsafe "new window['THREE']['Shape']()"
    thr_mkShape :: Three JSVal

mkShape :: Three Shape
mkShape = fromJSVal <$> thr_mkShape

foreign import javascript safe "($1)['holes']"
    thr_holes :: JSVal -> Three JSVal

-- | An array of paths that define the holes in the shape.
holes :: Shape -> Three [Path]
holes shape = (map fromJSVal . fromMaybe []) <$> (Marshal.fromJSVal =<< thr_holes (toJSVal shape))


foreign import javascript unsafe "($2)['holes'] = $1"
    thr_setHoles :: JSVal -> JSVal -> Three ()

setHoles :: [Path] -> Shape -> Three ()
setHoles ps shape = Marshal.toJSVal (map toJSVal ps) >>= flip thr_setHoles (toJSVal shape)
