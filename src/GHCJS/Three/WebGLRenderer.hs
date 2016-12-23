{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving, OverloadedStrings #-}
module GHCJS.Three.WebGLRenderer (
    WebGLRenderer(..), RendererOptionItem(..), RendererOption, mkWebGLRenderer,
    domElement, setSize, setViewport, setClearColor, setShadowMapEnabled, render,
    ShadowMapType, shadowMapTypeBasic, shadowMapTypePCF, shadowMapTypePCFSoft,
    setShadowMapType
) where

import GHCJS.Types
import qualified GHCJS.Marshal as Marshal
import GHCJS.DOM.Types (Element)
import JavaScript.Object

import Control.Monad

import GHCJS.Three.Monad
import GHCJS.Three.Scene
import GHCJS.Three.Camera

-- | webgl renderer definition
newtype WebGLRenderer = WebGLRenderer {
    glRendererObject :: BaseObject
} deriving (ThreeJSVal)

type JSRendererOption = Object

data RendererOptionItem = ROAlpha Bool
                        | ROAntialias Bool
                        | RODepth Bool

type RendererOption = [RendererOptionItem]

toJSVals :: RendererOptionItem -> IO (JSString, JSVal)
toJSVals (ROAlpha b) = toJSValsHelper "alpha" b
toJSVals (ROAntialias b) = toJSValsHelper "antialias" b
toJSVals (RODepth b) = toJSValsHelper "depth" b

toJSOption :: RendererOption -> IO JSRendererOption
toJSOption opts = do
    obj <- create
    forM_ opts (\item -> do
        (k, v) <- toJSVals item
        setProp k v obj)
    return obj

foreign import javascript unsafe "new window['THREE']['WebGLRenderer']($1)"
    thr_mkWebGLRenderer :: JSRendererOption -> Three JSVal

-- | create a new webgl renderer
mkWebGLRenderer :: RendererOption -> Three WebGLRenderer
mkWebGLRenderer opt = toJSOption opt >>= (fmap fromJSVal) . thr_mkWebGLRenderer

foreign import javascript unsafe "($1)['domElement']"
    thr_domElement :: JSVal -> Three JSVal
foreign import javascript unsafe "($3)['setSize']($1, $2)"
    thr_setSize :: Double -> Double -> JSVal -> Three ()
foreign import javascript unsafe "($5)['setViewport']($1, $2, $3, $4)"
    thr_setViewport :: Double -> Double -> Double -> Double -> JSVal -> Three ()
foreign import javascript unsafe "($3)['setClearColor']($1, $2)"
    thr_setClearColor :: Double -> Double -> JSVal -> Three ()
foreign import javascript unsafe "($3)['render']($1, $2)"
    thr_render :: JSVal -> JSVal -> JSVal -> Three ()
foreign import javascript unsafe "($2)['shadowMap']['enabled'] = $1 === 1"
    thr_setShadowMapEnabled :: Int -> JSVal -> Three ()


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

-- | set shadowMap.enabled
setShadowMapEnabled :: Bool -> WebGLRenderer -> Three ()
setShadowMapEnabled b r = thr_setShadowMapEnabled (if b then 1 else 0) (toJSVal r)

-- | set shadowMap.type
type ShadowMapType = Int

foreign import javascript unsafe "window['THREE']['BasicShadowMap']"
    shadowMapTypeBasic :: ShadowMapType

foreign import javascript unsafe "window['THREE']['PCFShadowMap']"
    shadowMapTypePCF :: ShadowMapType

foreign import javascript unsafe "window['THREE']['PCFSoftShadowMap']"
    shadowMapTypePCFSoft :: ShadowMapType

foreign import javascript unsafe "$2['shadowMap']['type'] = $1"
    thr_setShadowMapType :: Int -> JSVal -> Three ()

setShadowMapType :: ShadowMapType -> WebGLRenderer -> Three ()
setShadowMapType t r = thr_setShadowMapType t (toJSVal r)

-- | do render the scene
render :: IsCamera c => Scene -> c -> WebGLRenderer -> Three ()
render s c r = thr_render (toJSVal s) (toJSVal c) (toJSVal r)
