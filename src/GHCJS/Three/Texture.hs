{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Texture where

import Data.Functor
import GHCJS.Types
import GHCJS.Foreign.Callback
import GHCJS.Concurrent

import GHCJS.Three.Monad
import GHCJS.Three.Disposable

newtype Texture = Texture {
    getTextureObject :: BaseObject
} deriving (ThreeJSVal)

instance Disposable Texture

foreign import javascript unsafe "new window.THREE.Texture($1)"
    thr_mkTexture :: JSVal -> Three JSVal

mkTexture :: JSVal -> Three Texture
mkTexture img = fromJSVal <$> thr_mkTexture img

newtype ImageLoader = ImageLoader {
    getImageLoaderObject :: BaseObject
} deriving (ThreeJSVal)

foreign import javascript unsafe "new window.THREE.ImageLoader()"
    thr_mkImageLoader :: Three JSVal

mkImageLoader :: Three ImageLoader
mkImageLoader = fromJSVal <$> thr_mkImageLoader

foreign import javascript unsafe "($2).setCrossOrigin($1)"
    thr_setCrossOrigin :: JSString -> JSVal -> Three ()

setCrossOrigin :: JSString -> ImageLoader -> Three ()
setCrossOrigin url loader = thr_setCrossOrigin url $ toJSVal loader

foreign import javascript unsafe "($5).load($1, $2, $3, $4)"
    thr_load :: JSString -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> JSVal -> Three ()

loadImage :: JSString -> (BaseObject -> IO ()) -> (BaseObject -> IO ()) -> (BaseObject -> IO ()) -> ImageLoader -> Three ()
loadImage url onLoad onProgress onError loader = do
    loadCB <- syncCallback1 ContinueAsync (onLoad . fromJSVal)
    progCB <- syncCallback1 ContinueAsync (onProgress . fromJSVal)
    errCB  <- syncCallback1 ContinueAsync (onError . fromJSVal)
    thr_load url loadCB progCB errCB $ toJSVal loader
