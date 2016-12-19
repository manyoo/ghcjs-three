{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Box3 where

import GHCJS.Types

import GHCJS.Three.Monad

newtype Box3 = Box3 {
    box3Object :: BaseObject
    } deriving ThreeJSVal
