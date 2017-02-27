{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module GHCJS.Three.TextGeometry
    ( TextGeometry(..), mkTextGeometry, TextGeometryOptionItem(..), TextGeometryOptions
    ) where

import GHCJS.Types

import Data.JSString
import JavaScript.Object

import Control.Monad

import GHCJS.Three.Monad
import GHCJS.Three.Geometry
import GHCJS.Three.Disposable
import GHCJS.Three.Font

newtype TextGeometry = TextGeometry {
    textGeometry :: Geometry
    } deriving (ThreeJSVal, IsGeometry, HasBounding, Disposable)

foreign import javascript unsafe "new window['THREE']['TextGeometry']($1, $2)"
    thr_mkTextGeometry :: JSString -> JSVal -> Three JSVal

data TextGeometryOptionItem = TGFont Font
                            | TGSize Double
                            | TGHeight Double

type TextGeometryOptions = [TextGeometryOptionItem]

toJSVals :: TextGeometryOptionItem -> IO (JSString, JSVal)
toJSVals (TGFont f)   = toJSValsHelper "font" (toJSVal f)
toJSVals (TGSize s)   = toJSValsHelper "size" s
toJSVals (TGHeight h) = toJSValsHelper "height" h

toJSOption :: TextGeometryOptions -> IO Object
toJSOption opts = do
    obj <- create
    forM_ opts (\item -> do
                       (k, v) <- toJSVals item
                       setProp k v obj)
    return obj

mkTextGeometry :: String -> TextGeometryOptions -> Three TextGeometry
mkTextGeometry t o = (fmap fromJSVal . thr_mkTextGeometry (pack t) . jsval) =<< toJSOption o
