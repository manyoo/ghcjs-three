{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Path
    (Path(..), mkPath, IsPath(..)
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal

import GHCJS.Three.Monad
import GHCJS.Three.Vector

import Data.Vector.V2

newtype Path = Path {
    pathObject :: BaseObject
} deriving (ThreeJSVal)

foreign import javascript unsafe "new window['THREE']['Path']($1)"
    thr_mkPath :: JSVal -> Three JSVal

mkPath :: [Vector2] -> Three Path
mkPath points = mapM mkTVector2 points >>= Marshal.toJSVal . map toJSVal >>= fmap fromJSVal . thr_mkPath

foreign import javascript unsafe "($2)['fromPoints']($1)"
    thr_fromPoints :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($3)['moveTo']($1, $2)"
    thr_moveTo :: Double -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($3)['lineTo']($1, $2)"
    thr_lineTo :: Double -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($5)['quadraticCurveTo']($1, $2, $3, $4)"
    thr_quadraticCurveTo :: Double -> Double -> Double -> Double -> JSVal -> Three ()

foreign import javascript unsafe "($7)['bezierCurveTo']($1, $2, $3, $4, $5, $6)"
    thr_bezierCurveTo :: Double -> Double -> Double -> Double -> Double -> Double -> JSVal -> Three ()

class ThreeJSVal p => IsPath p where
    -- | Adds to the Path from the points. The first vector defines the offset. After that the lines get defined.
    pathFromPoints :: [Vector2] -> p -> Three ()
    pathFromPoints points path = mapM mkTVector2 points >>= Marshal.toJSVal . map toJSVal >>= flip thr_fromPoints (toJSVal path)

    -- | This moves the offset to x and y
    pathMoveTo :: Vector2 -> p -> Three ()
    pathMoveTo v p = thr_moveTo (v2x v) (v2y v) (toJSVal p)

    -- | This creates a line from the offset to X and Y and updates the offset to X and Y.
    pathLineTo :: Vector2 -> p -> Three ()
    pathLineTo v p = thr_lineTo (v2x v) (v2y v) (toJSVal p)

    -- | This creates a quadratic curve from the offset to x and y with cpX and cpY as control point and updates the offset to x and y.
    quadraticCurveTo :: Vector2 -> Vector2 -> p -> Three ()
    quadraticCurveTo cpV v p = thr_quadraticCurveTo (v2x cpV) (v2y cpV) (v2x v) (v2y v) (toJSVal p)

    -- | This creates a bezier curve from the last offset to x and y with cp1X, cp1Y and cp1X, cp1Y as control points and updates the offset to x and y.
    bezierCurveTo :: Vector2 -> Vector2 -> Vector2 -> p -> Three ()
    bezierCurveTo cpV1 cpV2 v p = thr_bezierCurveTo (v2x cpV1) (v2y cpV1) (v2x cpV2) (v2y cpV2) (v2x v) (v2y v) (toJSVal p)

instance IsPath Path
