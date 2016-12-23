{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Sphere (Sphere(..), mkSphere) where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Matrix
import GHCJS.Three.CanCopy

newtype Sphere = Sphere {
    sphereObject :: BaseObject
    } deriving ThreeJSVal

instance CanApplyMatrix4 Sphere
instance CanCopy Sphere

foreign import javascript unsafe "new window['THREE']['Sphere']()"
    thr_mkSphere :: Three JSVal

mkSphere :: Three Sphere
mkSphere = fromJSVal <$> thr_mkSphere
