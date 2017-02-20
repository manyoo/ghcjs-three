{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.TextGeometry
    ( TextGeometry(..), mkTextGeometry
    ) where

import GHCJS.Types

import Data.JSString
import JavaScript.Object

import GHCJS.Three.Monad
import GHCJS.Three.Geometry
import GHCJS.Three.Disposable

newtype TextGeometry = TextGeometry {
    textGeometry :: Geometry
    } deriving (ThreeJSVal, IsGeometry, HasBounding, Disposable)

foreign import javascript unsafe "new window['THREE']['TextGeometry']($1, $2)"
    thr_mkTextGeometry :: JSString -> JSVal -> Three JSVal

mkTextGeometry :: String -> Object -> Three TextGeometry
mkTextGeometry t p = fromJSVal <$> thr_mkTextGeometry (pack t) (jsval p)
