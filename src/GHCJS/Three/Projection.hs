{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Projection (project, unproject) where

import Data.Functor
import GHCJS.Types

import GHCJS.Three.Monad
import GHCJS.Three.Vector
import GHCJS.Three.Camera

-- public functions for JS Vector3
foreign import javascript unsafe "($2).project($1)"
    thr_project :: JSVal -> JSVal -> Three ()

project :: Camera -> TVector -> Three TVector
project c v = do
    jv <- mkVector v
    thr_project (toJSVal c) (toJSVal jv)
    return $ toTVector jv

foreign import javascript unsafe "($2).unproject($1)"
    thr_unproject :: JSVal -> JSVal -> Three ()

unproject :: Camera -> TVector -> Three TVector
unproject c v = do
    jv <- mkVector v
    thr_unproject (toJSVal c) (toJSVal jv)
    return $ toTVector jv
