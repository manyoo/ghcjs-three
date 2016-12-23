{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Face3 (
    Face3(..), mkFace3, verticeA, verticeB, verticeC, faceNormal, faceColor
    ) where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Vector
import GHCJS.Three.Color

newtype Face3 = Face3 {
    faceObject :: BaseObject
} deriving (ThreeJSVal)

-- create a new Face3 Object
foreign import javascript unsafe "new window['THREE']['Face3']($1, $2, $3, $4, $5, $6)"
    thr_mkFace3 :: Int -> Int -> Int -> JSVal -> JSVal -> Int -> Three JSVal

mkFace3 :: Int -> Int -> Int -> Vector3 -> TColor -> Int -> Three Face3
mkFace3 a b c nVec color matIdx = do
    nv <- mkTVector3 nVec
    cl <- toColor color
    fromJSVal <$> thr_mkFace3 a b c (toJSVal nv) (toJSVal cl) matIdx


-- varies functions to access the properties of a Face3 object
foreign import javascript unsafe "($1)['a']"
    thr_verticeA :: JSVal -> Three Int

verticeA :: Face3 -> Three Int
verticeA = thr_verticeA . toJSVal

foreign import javascript unsafe "($1)['b']"
    thr_verticeB :: JSVal -> Three Int

verticeB :: Face3 -> Three Int
verticeB = thr_verticeB . toJSVal

foreign import javascript unsafe "($1)['c']"
    thr_verticeC :: JSVal -> Three Int

verticeC :: Face3 -> Three Int
verticeC = thr_verticeC . toJSVal

foreign import javascript unsafe "($1)['normal']"
    thr_faceNormal :: JSVal -> Three JSVal

faceNormal :: Face3 -> Three Vector3
faceNormal f = (toVector3 . fromJSVal) =<< thr_faceNormal (toJSVal f)

foreign import javascript unsafe "($1)['color']"
    thr_faceColor :: JSVal -> Three JSVal

faceColor :: Face3 -> Three Color
faceColor = fmap fromJSVal . thr_faceColor . toJSVal
