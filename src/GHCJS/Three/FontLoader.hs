{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.FontLoader
    ( FontLoader(..), mkFontLoader, loadFont
    ) where

import GHCJS.Types

import qualified GHCJS.Marshal as M
import GHCJS.Foreign.Callback
import GHCJS.Concurrent

import GHCJS.Three.Monad

newtype FontLoader = FontLoader {
    fontLoaderObj :: BaseObject
    } deriving ThreeJSVal

foreign import javascript unsafe "new window['THREE']['FontLoader']()"
    thr_mkFontLoader :: Three JSVal

mkFontLoader :: Three FontLoader
mkFontLoader = fromJSVal <$> thr_mkFontLoader

foreign import javascript unsafe "($5)['load']($1, $2, $3, $4)"
    thr_loadFont :: JSString -> Callback (JSVal -> IO ()) -> Callback (JSVal -> IO ()) -> Callback (IO ()) -> JSVal -> Three ()

loadFont :: JSString -> (JSVal -> IO ()) -> (JSVal -> IO ()) -> IO () -> FontLoader -> Three ()
loadFont url onLoad onProg onError loader = do
    l <- syncCallback1 ThrowWouldBlock onLoad
    p <- syncCallback1 ThrowWouldBlock onProg
    e <- syncCallback ThrowWouldBlock onError
    thr_loadFont url l p e (toJSVal loader)
