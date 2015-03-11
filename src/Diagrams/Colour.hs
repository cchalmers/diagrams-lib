{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE DeriveFunctor              #-}
{-# LANGUAGE ExistentialQuantification  #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RankNTypes                 #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
{-# LANGUAGE ViewPatterns               #-}
{-# OPTIONS -fno-warn-orphans #-}
{-# OPTIONS -fno-warn-unused-imports #-}
import           Codec.Picture.Types
import           Control.Applicative
import           Control.Exception.Lens
import           Control.Lens             hiding (none, over)
import           Data.Colour
import           Data.Colour.CIE
import           Data.Colour.RGBSpace
import           Data.Colour.RGBSpace.HSL
import           Data.Colour.RGBSpace.HSV
import           Data.Colour.SRGB
import           Data.Typeable
import           Data.Word
import           System.IO.Unsafe

import           Diagrams.Core

-- | Traversal over the 'Colour' 'Double' of any instance of 'Color'.
--   Any 'transparent' 'Color' is ignored.
colour :: Color c => Traversal' c (Colour Double)
colour = iso toAlphaColour fromAlphaColour . color
  where
    color f ac
      | a == 0    = pure ac
      | otherwise = f (alphaToColour ac) <&> (`withOpacity` a)
      where a = alphaChannel ac
    -- An 'AlphaColour' with 0 alpha does not preserve it's colour, so
    -- this can't make a valid lens.


-- | Dirty function for catching exceptions in a pure function. Note
--   that @a@ is only evaluted to @whnf@ so any deeper errors are not
--   caught.
catchAllMaybe :: a -> Maybe a
catchAllMaybe a = unsafePerformIO $ trying_ id (return $! a)

-- | Prism on the hex form of a colour. This isn't a true Prism because
--   the hex is optional when reading but is always there when shown.
_Hex :: RealFloat a => Prism' String (Colour a)
_Hex = prism' sRGB24show (catchAllMaybe . sRGB24read)

------------------------------------------------------------
--  Color  -------------------------------------------------
------------------------------------------------------------

class HasColour a where
  _colour :: Lens' a (Colour Double)
  default _colour :: IsColour a => Lens' a (Colour Double)
  _colour = _Colour

class HasColour a => IsColour a where
  _Colour :: Iso' a (Colour Double)

class HasColour a => IsAlphaColour a where
  _AlphaColour :: Iso' a (AlphaColour Double)
  _PixelRGBA8  :: Iso' a PixelRGBA8

  _alpha :: Lens' a Double
  _alpha = _AlphaColour . alpha

-- colour colours ------------------------------------------------------

instance (Floating a, Real a) => HasColour (Colour a)
instance (Floating a, Real a) => IsColour (Colour a) where
  _Colour = iso colourConvert colourConvert

instance (Floating a, Real a) => HasColour (AlphaColour a) where
  _colour = color
instance (Floating a, Real a) => IsAlphaColour (AlphaColour a) where
  _AlphaColour = iso alphaColourConvert alphaColourConvert

------------------------------------------------------------------------
-- Class
------------------------------------------------------------------------

-- instance (Floating a, Real a) => Color (Colour a) where
--   toAlphaColour   = opaque . colourConvert
  -- fromAlphaColour = colourConvert . (`over` black)

-- $color
-- Diagrams outsources all things color-related to Russell O\'Connor\'s
-- very nice colour package
-- (<http://hackage.haskell.org/package/colour>).  For starters, it
-- provides a large collection of standard color names.  However, it
-- also provides a rich set of combinators for combining and
-- manipulating colors; see its documentation for more information.

-- | The 'Color' type class encompasses color representations which
--   can be used by the Diagrams library.  Instances are provided for
--   both the 'Data.Colour.Colour' and 'Data.Colour.AlphaColour' types
--   from the "Data.Colour" library.
class Color c where

  -- | Convert a color to its standard representation, AlphaColour.
  toAlphaColour :: c -> AlphaColour Double
  toAlphaColour = fromPixelRGBA8 . toPixelRGBA8

  -- | Convert from an AlphaColour Double.  Note that this direction
  --   may lose some information. For example, the instance for
  --   'Colour' drops the alpha channel.
  fromAlphaColour :: AlphaColour Double -> c
  fromAlphaColour = fromPixelRGBA8 . toPixelRGBA8

  toPixelRGBA8 :: c -> PixelRGBA8
  toPixelRGBA8 = fromAlphaColour . toAlphaColour

  fromPixelRGBA8 :: PixelRGBA8 -> c
  fromPixelRGBA8 = fromAlphaColour . toAlphaColour

------------------------------------------------------------------------
-- Lenses
------------------------------------------------------------------------

-- | Wrapper over 'AlphaColour' 'Double'.
-- data SomeColor = forall c. Color c => SomeColor c
--   deriving Typeable
newtype SomeColor = SomeColor (AlphaColour Double)
  deriving Typeable

alpha :: OrderedField a => Lens' (AlphaColour a) a
alpha f ac = f (alphaChannel ac) <&> \a -> alphaToColour ac `withOpacity` a

colour :: OrderedField a => Lens' (AlphaColour a) (Colour a)
colour f ac = f (alphaToColour ac) <&> \c -> c `withOpacity` alphaChannel ac

-- colour lenses -------------------------------------------------------

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (a,b,c) = f a b c

-- RGB -----------------------------------------------------------------

_RGB :: OrderedField a => Iso' (Colour a) (RGB a)
_RGB = iso toSRGB (uncurryRGB sRGB)

_rgb :: Color c => Lens' c (RGB Double)
_rgb = color . _RGB

_r, _g, _b :: Lens' (RGB a) a
_r f (RGB r g b) = f r <&> \r' -> RGB r' g  b
_g f (RGB r g b) = f g <&> \g' -> RGB r  g' b
_b f (RGB r g b) = f b <&> \b' -> RGB r  g  b'

_red, _green, _blue :: Color a => Lens' a Double
_red   = _rgb . _r
_green = _rgb . _g
_blue  = _rgb . _b

_RGB8 :: RealFloat a => Iso' (Colour a) (RGB Word8)
_RGB8 = iso toSRGB24 (uncurryRGB sRGB24)

_rgb8 :: Color c => Lens' c (RGB Word8)
_rgb8 = color . _RGB8

_pixelRgb8 :: Color c => Lens' c PixelRGB8
_pixelRgb8 f (toPixelRGBA8 -> PixelRGBA8 r g b a)
  = f (PixelRGB8 r g b) <&> \(PixelRGB8 r' g' b') -> fromPixelRGBA8 (PixelRGBA8 r' g' b' a)

_red8, _green8, _blue8 :: Color c => Lens' c Word8
_red8 f (toPixelRGBA8 -> PixelRGBA8 r g b a)
  = f r <&> \r' -> fromPixelRGBA8 (PixelRGBA8 r' g  b  a)
_green8 f (toPixelRGBA8 -> PixelRGBA8 r g b a)
  = f g <&> \g' -> fromPixelRGBA8 (PixelRGBA8 r  g' b  a)
_blue8 f (toPixelRGBA8 -> PixelRGBA8 r g b a)
  = f b <&> \b' -> fromPixelRGBA8 (PixelRGBA8 r  g  b' a)

_red8', _green8', _blue8' :: Color c => Lens' c Word8
_red8'   = _rgb8 . _r
_green8' = _rgb8 . _g
_blue8'  = _rgb8 . _b

-- Chromatic -----------------------------------------------------------

_CIE :: Fractional a => Iso' (Colour a) (a,a,a)
_CIE = iso cieXYZView (uncurry3 cieXYZ)

_cie :: Color c => Lens' c (Double,Double,Double)
_cie = color . _CIE

_chromaX, _chromaY, _chromaZ :: Color c => Lens' c Double
_chromaX = _cie . _1
_chromaY = _cie . _2
_chromaZ = _cie . _3

-- HSV -----------------------------------------------------------------

_HSV, _HSL :: RealFloat a => Iso' (Colour a) (a,a,a)
_HSV = _RGB . iso hsvView (uncurry3 hsv)
_HSL = _RGB . iso hslView (uncurry3 hsl)

_hsv, _hsl :: Color c => Lens' c (Double,Double,Double)
_hsv = _rgb . iso hsvView (uncurry3 hsv)
_hsl = _rgb . iso hslView (uncurry3 hsl)

_hue, _hsvSaturation, _hslSaturation, _hsvValue, _lightness :: Color c => Lens' c Double
_hue           = _hsv . _1
_hsvSaturation = _hsv . _2
_hsvValue      = _hsv . _3

_hslSaturation = _hsl . _2
_lightness     = _hsl . _3

-- CMYK ----------------------------------------------------------------

data CMYK a = CMYK !a !a !a !a
  deriving (Show, Read, Functor, Eq)

instance Applicative CMYK where
  pure a = CMYK a a a a
  CMYK f g h k <*> CMYK a b c d = CMYK (f a) (g b) (h c) (k d)

_c, _y, _m, _k :: Lens' (CMYK a) a
_c f (CMYK c m y k) = f c <&> \c' -> CMYK c' m  y  k
_m f (CMYK c m y k) = f m <&> \m' -> CMYK c  m' y  k
_y f (CMYK c m y k) = f y <&> \y' -> CMYK c  m  y' k
_k f (CMYK c m y k) = f k <&> \k' -> CMYK c  m  y  k'

_CMYK :: OrderedField a => Iso' (Colour a) (CMYK a)
_CMYK = _RGB . iso rgbtocmyk cmyktorgb
  where
    rgbtocmyk (RGB r g b)
     = CMYK ((k' - r) / k')
            ((k' - g) / k')
            ((k' - b) / k')
            k
      where k' = max r (max g b)
            k  = 1 - k'

    cmyktorgb (CMYK c m y k)
     = RGB ((1 - c) * (1 - k))
           ((1 - m) * (1 - k))
           ((1 - y) * (1 - k))

_cmyk :: Color c => Lens' c (CMYK Double)
_cmyk = color . _cmyk

_cyan, _magenta, _yellow, _black :: Color c => Lens' c Double
_cyan    = _cmyk . _c
_magenta = _cmyk . _m
_yellow  = _cmyk . _y
_black   = _cmyk . _k

someToAlpha :: SomeColor -> AlphaColour Double
someToAlpha (SomeColor c) = c

------------------------------------------------------------------------
-- Instances
------------------------------------------------------------------------

-- colour --------------------------------------------------------------

instance (Floating a, Real a) => Color (Colour a) where
  toAlphaColour   = opaque . colourConvert
  fromAlphaColour = colourConvert . (`over` black)

instance (Floating a, Real a) => Color (AlphaColour a) where
  toAlphaColour   = alphaColourConvert
  fromAlphaColour = alphaColourConvert

instance Color (RGB Word8) where
  toAlphaColour   = opaque . review _RGB8
  fromAlphaColour = view _rgb8

instance Color (RGB Float) where
  toAlphaColour   = opaque . review _RGB . fmap r2f
  fromAlphaColour = fmap r2f . view _rgb

instance Color (RGB Double) where
  toAlphaColour   = opaque . review _RGB
  fromAlphaColour = view _rgb

-- JuicyPixels ---------------------------------------------------------

instance Color PixelRGB8 where
  toAlphaColour (PixelRGB8 r g b) = opaque $ sRGB24 r g b
  fromAlphaColour c               = PixelRGB8 r g b
    where (r,g,b,_) = colorToSRGBABounded c
  toPixelRGBA8   = promotePixel

instance Color PixelRGB16 where
  toAlphaColour (PixelRGB16 r g b) = opaque $ sRGBBounded r g b
  fromAlphaColour (colorToSRGBABounded -> (r,g,b,_)) = PixelRGB16 r g b

instance Color PixelRGBA8 where
  toAlphaColour (PixelRGBA8 r g b a) = sRGB24 r g b `withOpacity` o
    where o = fromIntegral a / 256
  fromAlphaColour c                  = PixelRGBA8 r g b a
    where (r,g,b,a) = colorToSRGBABounded c
  toPixelRGBA8   = id
  fromPixelRGBA8 = id

instance Color PixelRGBA16 where
  toAlphaColour (PixelRGBA16 r g b a) = sRGBBounded r g b `withOpacity` o
    where o = fromIntegral a / 256
  fromAlphaColour c                   = PixelRGBA16 r g b a
    where (r,g,b,a) = colorToSRGBABounded c
  -- toPixelRGBA8  = convertPixel
  -- fromPixelRGBA8 = convertPixel

instance Color PixelRGBF where
  toAlphaColour (PixelRGBF r g b) = opaque $ sRGB (r2f r) (r2f g) (r2f b)
  fromAlphaColour c               = PixelRGBF (r2f r) (r2f g) (r2f b)
    where (r,g,b,_) = colorToSRGBA c

-- cymk ----------------------------------------------------------------

instance Color PixelCMYK8 where
  toAlphaColour cmyk = toAlphaColour rgb
    where rgb = convertPixel cmyk :: PixelRGB8
  fromAlphaColour c = convertPixel rgb
    where rgb = fromAlphaColour c :: PixelRGB8
  fromPixelRGBA8 = convertPixel . dropTransparency

instance Color PixelCMYK16 where
  toAlphaColour cmyk = toAlphaColour rgb
    where rgb = convertPixel cmyk :: PixelRGB16
  fromAlphaColour c = convertPixel rgb
    where rgb = fromAlphaColour c :: PixelRGB16

-- instance Color (CMYK Word8) where
-- instance Color (CMYK Float) where
-- instance Color (CMYK Double) where


instance Color SomeColor where
  toAlphaColour (SomeColor c) = c
  fromAlphaColour             = SomeColor

------------------------------------------------------------------------
-- Utilities
------------------------------------------------------------------------

-- | Convert to sRGBA.
colorToSRGBA, colorToRGBA :: Color c => c -> (Double, Double, Double, Double)
colorToSRGBA col = (r, g, b, a)
  where
    c' = toAlphaColour col
    c = alphaToColour c'
    a = alphaChannel c'
    RGB r g b = toSRGB c

colorToRGBA = colorToSRGBA
{-# DEPRECATED colorToRGBA "Renamed to colorToSRGBA." #-}

colorToSRGBABounded :: (Bounded a, Integral a, Color c) => c -> (a, a, a, a)
colorToSRGBABounded (toAlphaColour -> ac) = (r, g, b, a')
  where
    c = alphaToColour ac
    a = alphaChannel ac
    a' = round $ a * fromIntegral (maxBound `asTypeOf` a')
    RGB r g b = toSRGBBounded c

alphaToColour :: (Floating a, Ord a, Fractional a) => AlphaColour a -> Colour a
alphaToColour ac
  | a == 0    = ac `over` black
  | otherwise = darken (recip a) (ac `over` black)
  where a = alphaChannel ac

r2f :: (Real a, Fractional b) => a -> b
r2f = realToFrac
{-# INLINE r2f #-}

