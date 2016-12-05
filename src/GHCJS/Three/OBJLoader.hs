{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module GHCJS.Three.OBJLoader (
    OBJLoader(..), mkOBJLoader, setOBJPath, setMaterials, loadOBJ
    ) where

import GHCJS.Types
import qualified GHCJS.Marshal as M
import GHCJS.Foreign.Callback
import GHCJS.Concurrent
import JavaScript.Object
import JavaScript.Object.Internal

import GHCJS.Three.Monad
import GHCJS.Three.Object3D (Object3D)
import GHCJS.Three.MTLLoader (MaterialCreator)

import Control.Monad
import Data.Maybe

newtype OBJLoader = OBJLoader {
    objLoaderObj :: BaseObject
    } deriving ThreeJSVal

foreign import javascript unsafe "new window['THREE']['OBJLoader']()"
    thr_mkOBJLoader :: Three JSVal

mkOBJLoader :: Three OBJLoader
mkOBJLoader = fromJSVal <$> thr_mkOBJLoader

foreign import javascript unsafe "($2)['setPath']($1)"
    thr_setPath :: JSString -> JSVal -> Three ()

setOBJPath :: JSString -> OBJLoader -> Three ()
setOBJPath p l = thr_setPath p (toJSVal l)

foreign import javascript unsafe "($2)['setMaterials']($1)"
    thr_setMaterials :: JSVal -> JSVal -> Three ()

setMaterials :: MaterialCreator -> OBJLoader -> Three ()
setMaterials m l = thr_setMaterials (toJSVal m) (toJSVal l)

foreign import javascript unsafe "($5)['load']($1, $2, $3, $4)"
    thr_loadOBJ :: JSString -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> Callback (IO ()) -> JSVal -> Three ()

loadOBJ :: JSString -> (Object3D -> IO ()) -> (Double -> IO ()) -> IO () -> OBJLoader -> Three ()
loadOBJ url onLoad onProgress onError loader = do
    let toProg v = do
            let o = Object v
            t <- M.fromJSVal =<< getProp "total" o
            l <- M.fromJSVal =<< getProp "loaded" o
            return $ fromMaybe 0 l / fromMaybe 1 t
    l <- syncCallback1 ThrowWouldBlock (onLoad . fromJSVal)
    p <- syncCallback1 ThrowWouldBlock (onProgress <=< toProg)
    e <- syncCallback ThrowWouldBlock onError
    thr_loadOBJ url l p e (toJSVal loader)
