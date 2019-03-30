module Utils where

import Debug.Trace
import Graphics.Gloss

type Angle  = Float
type Coords = (Float, Float)
type Size   = Float
type Size'  = (Size , Size)
type Step   = Int

trace' x = trace (show x) x

getColor :: Step -> Step -> Color -> Color -> Color
{-^ Fade colours for sun
    Current Step -> Total Steps -> Starting Color -> Eding Color
      -> Resulting colour-}
getColor s t = mixColors (100 - s') s'
  where
    s' = fromIntegral (s `mod` t) * 100 / fromIntegral t