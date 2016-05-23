{-# LANGUAGE JavaScriptFFI, GeneralizedNewtypeDeriving #-}
module GHCJS.Three.Color (
    Color(..), mkColor, IsColor(..), HasColor(..), TColor(..), toColor, fromColor
    ) where

import GHCJS.Types
import GHCJS.Three.Monad

newtype Color = Color {
    colorObject :: BaseObject
} deriving (ThreeJSVal)

type Red = Double
type Green = Double
type Blue = Double

-- | create a new color object with r,g,b values
foreign import javascript unsafe "new window['THREE']['Color']($1, $2, $3)"
    thr_mkColor :: Red -> Green -> Blue -> Three JSVal

mkColor :: Red -> Green -> Blue -> Three Color
mkColor r g b = fromJSVal <$> thr_mkColor r g b

-- | get red
foreign import javascript safe "($1)['r']"
    thr_red :: JSVal -> Red

-- | set red
foreign import javascript unsafe "($2)['r'] = $1"
    thr_setRed :: Red -> JSVal -> Three ()

-- | get green
foreign import javascript safe "($1)['g']"
    thr_green :: JSVal -> Green

-- | set green
foreign import javascript unsafe "($2)['g'] = $1"
    thr_setGreen :: Green -> JSVal -> Three ()

-- | get blue
foreign import javascript safe "($1)['b']"
    thr_blue :: JSVal -> Blue

-- | set blue
foreign import javascript unsafe "($2)['b'] = $1"
    thr_setBlue :: Blue -> JSVal -> Three ()

-- | setRGB
foreign import javascript unsafe "($4)['setRGB']($1, $2, $3)"
    thr_setRGB :: Red -> Green -> Blue -> JSVal -> Three ()

-- generic function to get/set color for objects
foreign import javascript safe "($1)['color']"
    thr_color :: JSVal -> JSVal

foreign import javascript unsafe "($2)['color'] = $1"
    thr_setColor :: JSVal -> JSVal -> Three ()

class ThreeJSVal c => IsColor c where
    red :: c -> Red
    red = thr_red . toJSVal

    setRed :: Red -> c -> Three ()
    setRed r c = thr_setRed r $ toJSVal c

    green :: c -> Green
    green = thr_green . toJSVal

    setGreen :: Green -> c -> Three ()
    setGreen g c = thr_setGreen g $ toJSVal c

    blue :: c -> Blue
    blue = thr_blue . toJSVal

    setBlue :: Blue -> c -> Three ()
    setBlue b c = thr_setBlue b $ toJSVal c

    setRGB :: Red -> Green -> Blue -> c -> Three ()
    setRGB r g b c = thr_setRGB r g b $ toJSVal c

instance IsColor Color

class ThreeJSVal o =>  HasColor o where
    -- | get color object
    color :: o -> Color
    color = fromJSVal . thr_color . toJSVal

    setColor :: Color -> o -> Three ()
    setColor c o = thr_setColor (toJSVal c) (toJSVal o)


-- | define a TColor type to be used by users easily
data TColor = TColor Red Green Blue

toColor :: TColor -> Three Color
toColor (TColor r g b) = mkColor r g b

fromColor :: Color -> TColor
fromColor c = TColor (red c) (green c) (blue c)
