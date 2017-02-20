{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Font
    ( Font(..), mkFont, fontData, isFont
    ) where

import GHCJS.Types

import GHCJS.Three.Monad

newtype Font = Font {
    fontObject :: BaseObject
    } deriving ThreeJSVal

foreign import javascript unsafe "new window['THREE']['Font']($1)"
    thr_mkFont :: JSVal -> Three JSVal

mkFont :: JSVal -> Three Font
mkFont = fmap fromJSVal . thr_mkFont

foreign import javascript unsafe "($1)['data']"
    thr_fontData :: JSVal -> Three JSVal

fontData :: Font -> Three JSVal
fontData = thr_fontData . toJSVal

foreign import javascript unsafe "($1)['isFont']"
    thr_isFont :: JSVal -> Three Bool

isFont :: Font -> Three Bool
isFont = thr_isFont . toJSVal
