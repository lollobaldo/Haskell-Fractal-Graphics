module Sun where

import Graphics.Gloss
import Utils

data Sun = Sun {
  coords :: Coords,
  period :: Float,
  size :: Float,
  steps :: Step,
  time :: Float
}

sun :: Picture
sun = circleSolid 100

sun :: Sun -> Picture
sun Sun {coords=c, period=p, size=sz, steps=st, time=t} =
    pictures $
    map (color cl . rotate dg . sunSolid) [1..st]
  where
    cl = getColor 
    dg = 0
    step = t `mod'` p * st / period

sunSolid :: Size -> Picture
sunSolid side = pictures [
    polygon [(-si , -i),
              (0    , h-i),
              (si   , -i)] ,
    polygon [(-si , i  ),
              (si   , i  ),
              (0    , s-i)]]
  where
    si = side / 2
    s = side / 3 * (-0.86602540378)
    t = side * 0.57735026919
    i = t / 2
    h = side * 0.86602540378

nextColour :: Step -> Color
{-Fade colours for sun -- Steps left -> Resulting colour-}
nextColour st = mixColors (100 - stepp) stepp myRed myYellow
  where
    stepp = fromIntegral st * 100 / fromIntegral stepsSun

coloursSun :: [Color]
coloursSun = cycle $ tail list ++ tail (reverse list)
  where
    list  = [mixColors (100 - stepp) stepp myRed myYellow | stepp <- percs]
    percs = [fromIntegral steppp * 100 / fromIntegral stepsSun | steppp <- [0..stepsSun]]