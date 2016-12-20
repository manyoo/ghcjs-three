{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Sphere where

import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Matrix
import GHCJS.Three.CanCopy

newtype Sphere = Sphere {
    sphereObject :: BaseObject
    } deriving ThreeJSVal

instance CanApplyMatrix4 Sphere
instance CanCopy Sphere
