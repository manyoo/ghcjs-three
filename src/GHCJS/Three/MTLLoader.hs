{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module GHCJS.Three.MTLLoader (MaterialCreator(..), mkMaterialCreator, preload,
    MTLLoader(..), mkMTLLoader, setMTLPath, setTexturePath, setMTLLoaderCrossOrigin,
    loadMTL) where

import GHCJS.Types
import qualified GHCJS.Marshal as M
import GHCJS.Foreign.Callback
import GHCJS.Concurrent
import JavaScript.Object
import JavaScript.Object.Internal

import GHCJS.Three.Monad
import GHCJS.Three.Object3D (Object3D)

import Control.Monad
import Data.Maybe

newtype MaterialCreator = MaterialCreator {
    materialCreatorObj :: BaseObject
    } deriving ThreeJSVal

foreign import javascript unsafe "new window['THREE']['MaterialLoader']()"
    thr_mkMaterialCreator :: Three JSVal

mkMaterialCreator :: Three MaterialCreator
mkMaterialCreator = fromJSVal <$> thr_mkMaterialCreator

foreign import javascript unsafe "($1)['preload']()"
    thr_preload :: JSVal -> Three ()

preload :: MaterialCreator -> Three ()
preload = thr_preload . toJSVal

newtype MTLLoader = MTLLoader {
    mtlLoaderObj :: BaseObject
    } deriving ThreeJSVal

foreign import javascript unsafe "new window['THREE']['MTLLoader']()"
    thr_mkMTLLoader :: Three JSVal

mkMTLLoader :: Three MTLLoader
mkMTLLoader = fromJSVal <$> thr_mkMTLLoader

foreign import javascript unsafe "($2)['setPath']($1)"
    thr_setPath :: JSString -> JSVal -> Three ()

setMTLPath :: JSString -> MTLLoader -> Three ()
setMTLPath p l = thr_setPath p (toJSVal l)

foreign import javascript unsafe "($2)['setTexturePath']($1)"
    thr_setTexturePath :: JSString -> JSVal -> Three ()

setTexturePath :: JSString -> MTLLoader -> Three ()
setTexturePath p l = thr_setTexturePath p (toJSVal l)

foreign import javascript unsafe "($2)['setCrossOrigin']($1)"
    thr_setCrossOrigin :: Bool -> JSVal -> Three ()

setMTLLoaderCrossOrigin :: Bool -> MTLLoader -> Three ()
setMTLLoaderCrossOrigin c l = thr_setCrossOrigin c (toJSVal l)

foreign import javascript unsafe "($5)['load']($1, $2, $3, $4)"
    thr_loadMTL :: JSString -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> Callback (IO ()) -> JSVal -> Three ()

loadMTL :: JSString -> (MaterialCreator -> IO ()) -> (Double -> IO ()) -> IO () -> MTLLoader -> Three ()
loadMTL url onLoad onProgress onError loader = do
    let toProg v = do
            let o = Object v
            t <- M.fromJSVal =<< getProp "total" o
            l <- M.fromJSVal =<< getProp "loaded" o
            return $ fromMaybe 0 l / fromMaybe 1 t
    l <- syncCallback1 ThrowWouldBlock (onLoad . fromJSVal)
    p <- syncCallback1 ThrowWouldBlock (onProgress <=< toProg)
    e <- syncCallback ThrowWouldBlock onError
    thr_loadMTL url l p e (toJSVal loader)
