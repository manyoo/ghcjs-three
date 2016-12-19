{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Sphere where

import GHCJS.Types

import GHCJS.Three.Monad

newtype Sphere = Sphere {
    sphereObject :: BaseObject
    } deriving ThreeJSVal
