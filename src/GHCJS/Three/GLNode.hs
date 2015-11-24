{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.GLNode (
    GLNode(..), IsGLNode(..), add, remove
) where

import GHCJS.Types

import GHCJS.Three.Monad

foreign import javascript unsafe "($2).add($1)"
    thr_add :: JSVal -> JSVal -> Three ()

foreign import javascript unsafe "($2).remove($1)"
    thr_remove :: JSVal -> JSVal -> Three ()

newtype GLNode = GLNode BaseObject
    deriving (ThreeJSVal)

class ThreeJSVal n => IsGLNode n where
    toGLNode :: n -> GLNode
    toGLNode = fromJSVal . toJSVal
    fromGLNode :: GLNode -> n
    fromGLNode = fromJSVal . toJSVal

-- | add child object c to parent p
add :: GLNode -> GLNode -> Three ()
add c p = thr_add (toJSVal c) (toJSVal p)

-- | remove child object c from parent p
remove :: GLNode -> GLNode -> Three ()
remove c p = thr_remove (toJSVal c) (toJSVal p)
