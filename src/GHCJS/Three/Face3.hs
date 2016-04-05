{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Face3 where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Vector
import GHCJS.Three.Color

newtype Face3 = Face3 {
    faceObject :: BaseObject
} deriving (ThreeJSVal)

-- create a new Face3 Object
foreign import javascript unsafe "new window.THREE.Face3($1, $2, $3, $4, $5, $6)"
    thr_mkFace3 :: Int -> Int -> Int -> JSVal -> JSVal -> Int -> Three JSVal

mkFace3 :: Int -> Int -> Int -> Vector3 -> Color -> Int -> Three Face3
mkFace3 a b c nVec color matIdx = do
    nv <- mkTVector3 nVec
    fromJSVal <$> thr_mkFace3 a b c (toJSVal nv) (toJSVal color) matIdx


-- varies functions to access the properties of a Face3 object
foreign import javascript safe "($1).a"
    thr_verticeA :: JSVal -> Int

verticeA :: Face3 -> Int
verticeA = thr_verticeA . toJSVal

foreign import javascript safe "($1).b"
    thr_verticeB :: JSVal -> Int

verticeB :: Face3 -> Int
verticeB = thr_verticeB . toJSVal

foreign import javascript safe "($1).c"
    thr_verticeC :: JSVal -> Int

verticeC :: Face3 -> Int
verticeC = thr_verticeC . toJSVal

foreign import javascript safe "($1).normal"
    thr_faceNormal :: JSVal -> JSVal

faceNormal :: Face3 -> Vector3
faceNormal = toVector3 . fromJSVal . thr_faceNormal . toJSVal

foreign import javascript safe "($1).color"
    thr_faceColor :: JSVal -> JSVal

faceColor :: Face3 -> Color
faceColor = fromJSVal . thr_faceColor . toJSVal
