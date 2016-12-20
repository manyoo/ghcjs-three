{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.BufferGeometry where

import GHCJS.Types
import JavaScript.Array
import JavaScript.Array.Internal

import GHCJS.Three.Monad
import GHCJS.Three.Geometry
import GHCJS.Three.Disposable

newtype BufferAttribute = BufferAttribute {
    bufferAttributeObject :: BaseObject
    } deriving ThreeJSVal

foreign import javascript unsafe "($1)['array']"
    thr_array :: JSVal -> JSVal

array :: BufferAttribute -> JSArray
array = SomeJSArray . thr_array . toJSVal

foreign import javascript unsafe "($1)['count']"
    thr_count :: JSVal -> Int

count :: BufferAttribute -> Int
count = thr_count . toJSVal


newtype BufferGeometry = BufferGeometry {
    bufferGeometryObject :: BaseObject
    } deriving ThreeJSVal

instance HasBounding BufferGeometry
instance Disposable BufferGeometry

foreign import javascript unsafe "new window['THREE']['BufferGeometry']()"
    thr_mkBufferGeometry :: Three JSVal

mkBufferGeometry :: Three BufferGeometry
mkBufferGeometry = fromJSVal <$> thr_mkBufferGeometry

maybeAttribute :: JSVal -> Maybe BufferAttribute
maybeAttribute v = if isNull v
                   then Nothing
                   else Just (fromJSVal v)

foreign import javascript unsafe "($2)['getAttribute']($1)"
    thr_getAttribute :: JSString -> JSVal -> JSVal

getAttribute :: JSString -> BufferGeometry -> Maybe BufferAttribute
getAttribute n g = maybeAttribute $ thr_getAttribute n (toJSVal g)

foreign import javascript unsafe "($1)['getIndex']()"
    thr_getIndex :: JSVal -> JSVal

getIndex :: BufferGeometry -> Maybe BufferAttribute
getIndex = maybeAttribute . thr_getIndex . toJSVal
