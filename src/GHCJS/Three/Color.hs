{-# LANGUAGE JavaScriptFFI #-}
module GHCJS.Three.Color where

import GHCJS.Types
import GHCJS.Three.Monad

data CColor a
type Color a = Object (CColor a)

type Red = Double
type Green = Double
type Blue = Double

-- | create a new color object with r,g,b values
foreign import javascript unsafe "new window.THREE.Color($1, $2, $3)"
    mkColor :: Red -> Green -> Blue -> Three (Color ())

-- | get red
foreign import javascript safe "($1).r"
    red :: Color a -> Red

-- | set red
foreign import javascript unsafe "($2).r = $1"
    setRed :: Red -> Color a -> Three ()

-- | get green
foreign import javascript safe "($1).g"
    green :: Color a -> Green

-- | set green
foreign import javascript unsafe "($2).g = $1"
    setGreen :: Green -> Color a -> Three ()

-- | get blue
foreign import javascript safe "($1).b"
    blue :: Color a -> Blue

-- | set blue
foreign import javascript unsafe "($2).b = $1"
    setBlue :: Blue -> Color a -> Three ()

-- | setRGB
foreign import javascript unsafe "($4).setRGB($1, $2, $3)"
    setRGB :: Red -> Green -> Blue -> Color a -> Three ()

-- generic function to get/set color for objects
foreign import javascript safe "($1).color"
    objColor :: JSRef -> Color ()

foreign import javascript unsafe "($2).color = $1"
    objSetColor :: Color a -> JSRef -> Three ()

class IsJSRef o =>  HasColor o where
    -- | get color object
    color :: o -> Color ()
    color = objColor . jsref

    setColor :: Color a -> o -> Three ()
    setColor c o = objSetColor c (jsref o)
