module Utils where

type Angle  = Float
type Coords = (Float, Float)
type Size   = Float
type Size'  = (Size , Size)
type Step   = Int

getColor :: Step -> Step -> Color -> Color -> Color
{-Fade colours for sun -- Steps left -> Resulting colour-}
getColor s t = mixColors (100 - s') s'
  where
    s' = fromIntegral s * 100 / fromIntegral t