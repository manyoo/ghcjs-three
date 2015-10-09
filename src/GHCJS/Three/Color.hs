{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Color (
    Color(..), mkColor, IsColor(..), HasColor(..)
    ) where

import GHCJS.Types
import GHCJS.Three.Monad

newtype Color = Color {
    colorObject :: Object
} deriving (ThreeJSRef)

type Red = Double
type Green = Double
type Blue = Double

-- | create a new color object with r,g,b values
foreign import javascript unsafe "new window.THREE.Color($1, $2, $3)"
    thr_mkColor :: Red -> Green -> Blue -> Three JSRef

mkColor :: Red -> Green -> Blue -> Three Color
mkColor r g b = fromJSRef <$> thr_mkColor r g b

-- | get red
foreign import javascript safe "($1).r"
    thr_red :: JSRef -> Red

-- | set red
foreign import javascript unsafe "($2).r = $1"
    thr_setRed :: Red -> JSRef -> Three ()

-- | get green
foreign import javascript safe "($1).g"
    thr_green :: JSRef -> Green

-- | set green
foreign import javascript unsafe "($2).g = $1"
    thr_setGreen :: Green -> JSRef -> Three ()

-- | get blue
foreign import javascript safe "($1).b"
    thr_blue :: JSRef -> Blue

-- | set blue
foreign import javascript unsafe "($2).b = $1"
    thr_setBlue :: Blue -> JSRef -> Three ()

-- | setRGB
foreign import javascript unsafe "($4).setRGB($1, $2, $3)"
    thr_setRGB :: Red -> Green -> Blue -> JSRef -> Three ()

-- generic function to get/set color for objects
foreign import javascript safe "($1).color"
    thr_color :: JSRef -> JSRef

foreign import javascript unsafe "($2).color = $1"
    thr_setColor :: JSRef -> JSRef -> Three ()

class ThreeJSRef c => IsColor c where
    red :: c -> Red
    red = thr_red . toJSRef

    setRed :: Red -> c -> Three ()
    setRed r c = thr_setRed r $ toJSRef c

    green :: c -> Green
    green = thr_green . toJSRef

    setGreen :: Green -> c -> Three ()
    setGreen g c = thr_setGreen g $ toJSRef c

    blue :: c -> Blue
    blue = thr_blue . toJSRef

    setBlue :: Blue -> c -> Three ()
    setBlue b c = thr_setBlue b $ toJSRef c

    setRGB :: Red -> Green -> Blue -> c -> Three ()
    setRGB r g b c = thr_setRGB r g b $ toJSRef c

instance IsColor Color

class ThreeJSRef o =>  HasColor o where
    -- | get color object
    color :: o -> Color
    color = fromJSRef . thr_color . toJSRef

    setColor :: Color -> o -> Three ()
    setColor c o = thr_setColor (toJSRef c) (toJSRef o)
