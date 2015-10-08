module GHCJS.Three.Monad (
    Three,
    Object(..)
) where

import GHCJS.Types (JSRef)

type Three = IO
type Object a = JSRef
