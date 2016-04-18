{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.ShapeGeometry
    (
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal

import GHCJS.Three.Monad
import GHCJS.Three.Geometry
import GHCJS.Three.Shape
import GHCJS.Three.Disposable

newtype ShapeGeometry = ShapeGeometry {
    getShapeGeometry :: Geometry
} deriving (ThreeJSVal, IsGeometry, Disposable)

foreign import javascript unsafe "new window.THREE.ShapeGeometry($1)"
    thr_mkShapeGeometry :: JSVal -> Three JSVal

data ShapeParam = SingleShape Shape
                | ShapeList [Shape]

spToJSVal :: ShapeParam -> IO JSVal
spToJSVal (SingleShape s) = return $ toJSVal s
spToJSVal (ShapeList ss)  = Marshal.toJSVal $ map toJSVal ss

mkShapeGeometry :: ShapeParam -> Three ShapeGeometry
mkShapeGeometry sp = spToJSVal sp >>= fmap fromJSVal . thr_mkShapeGeometry
