module Sun where

import Data.Fixed
import Graphics.Gloss
import Utils

data Sun = Sun {
  colorStart :: Color,
  colorEnd :: Color,
  coords :: Coords,
  period :: Float,
  size :: Float,
  steps :: Step,
  time :: Float
}

sun :: Sun -> Picture
sun Sun {colorStart=c1, colorEnd=c2, coords=c, period=p, size=sz, steps=st,
  time=t} =
    pictures . reverse $
    --map (color cl . rotate dg . sunSolid . getSize . fromIntegral) [1..st]
    [sunPart x
      | x <- [1..st]]
    -- map (color cl . sunSolid . getSize . fromIntegral) [1..st]
  where
    sunPart :: Int -> Picture
    sunPart i = (rotate (dg i)
              . color (drop step colors !! i )
              . sunSolid
              . getSize
              . fromIntegral) i
    cl s = getColor s st c2 c1
    dg = (30*) . fromIntegral
    getSize = (/st') . (*sz)
    step = round $ (t `mod'` p) * 2*st' / p
    st' = fromIntegral st
    colors = sunColors st c1 c2

sunColors :: Step -> Color -> Color -> [Color]
sunColors st c2 c1 = cycle $ ls ++ reverse ls
  where
    ls = [mixColors (100-x) x c1 c2 | let d = 100/fromIntegral st, x <- [0,d..100]]

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