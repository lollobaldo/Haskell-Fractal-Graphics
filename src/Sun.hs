module Sun (
  Sun(..),
  sun,
  stdSun) where

import Data.Fixed
import Graphics.Gloss

-- import Scene
import Utils

data Sun = Sun {
  sunColorStart :: Color,
  sunColorEnd :: Color,
  sunCoords :: Coords,
  sunPeriod :: Float,
  sunSize :: Float,
  sunSteps :: Step,
  sunTime :: Float
}

sun :: Sun -> Picture
sun Sun {sunColorStart=c1, sunColorEnd=c2, sunCoords=c,
         sunPeriod=p, sunSize=sz, sunSteps=st, sunTime=t} =
    uncurry translate c . pictures .map sunPart $ [st,st-1..1]
  where
    sunPart :: Int -> Picture
    sunPart i = (rotate (dg i)
              . color (drop step colors !! i )
              . sunSolid
              . getSize
              . fromIntegral) i
    -- cl s = getColor s st c2 c1
    dg = (30*) . fromIntegral
    getSize = (/st') . (*sz)
    step = round $ (t `mod'` p) * 2*st' / p
    st' = fromIntegral st
    colors = sunColors st c1 c2
-- sun _ = error "Cannot initialise as Sun"

sunColors :: Step -> Color -> Color -> [Color]
sunColors st c2 c1 = cycle $ ls ++ (tail . reverse) ls
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

stdSun :: Sun
stdSun = Sun {
  sunColorStart = red,
  sunColorEnd = yellow,
  sunCoords = (-200, 300),
  sunPeriod = 3,
  sunSize = 100,
  sunSteps = 15,
  sunTime = 0
}