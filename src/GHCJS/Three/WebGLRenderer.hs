{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.WebGLRenderer (
    WebGLRenderer(..), mkWebGLRenderer, domElement, setSize, setViewport,
    setClearColor, render
) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal
import GHCJS.DOM.Types (Element)

import GHCJS.Three.Monad
import GHCJS.Three.Scene
import GHCJS.Three.Camera

-- | webgl renderer definition
newtype WebGLRenderer = WebGLRenderer {
    glRendererObject :: Object
} deriving (ThreeJSVal)

foreign import javascript unsafe "new window.THREE.WebGLRenderer()"
    thr_mkWebGLRenderer :: Three JSVal

-- | create a new webgl renderer
mkWebGLRenderer :: Three WebGLRenderer
mkWebGLRenderer = fromJSVal <$> thr_mkWebGLRenderer

foreign import javascript unsafe "($1).domElement"
    thr_domElement :: JSVal -> Three JSVal
foreign import javascript unsafe "($3).setSize($1, $2)"
    thr_setSize :: Double -> Double -> JSVal -> Three ()
foreign import javascript unsafe "($5).setViewport($1, $2, $3, $4)"
    thr_setViewport :: Double -> Double -> Double -> Double -> JSVal -> Three ()
foreign import javascript unsafe "($3).setClearColor($1, $2)"
    thr_setClearColor :: Double -> Double -> JSVal -> Three ()
foreign import javascript unsafe "($3).render($1, $2)"
    thr_render :: JSVal -> JSVal -> JSVal -> Three ()

-- | get the dom element (canvas) of the output
domElement :: WebGLRenderer -> Three (Maybe Element)
domElement r = (thr_domElement (toJSVal r) >>= Marshal.fromJSVal)

-- | set size of the output canvas
setSize :: Double -> Double -> WebGLRenderer -> Three ()
setSize w h r = thr_setSize w h $ toJSVal r

-- | set viewport to render
setViewport :: Double -> Double -> Double -> Double -> WebGLRenderer -> Three ()
setViewport x y w h r = thr_setViewport x y w h $ toJSVal r

-- | set clear color
setClearColor :: Double -> Double -> WebGLRenderer -> Three ()
setClearColor color alpha r = thr_setClearColor color alpha $ toJSVal r

-- | do render the scene
render :: IsCamera c => Scene -> c -> WebGLRenderer -> Three ()
render s c r = thr_render (toJSVal s) (toJSVal c) (toJSVal r)
