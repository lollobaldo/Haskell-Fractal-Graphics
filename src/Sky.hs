module Sky (
  Sky(..),
  sky,
  stdSky) where

import Data.Fixed
import Graphics.Gloss
import Utils

data Sky = Sky {
  skyColorMain :: Color,
  skySize :: Size'
}

sky :: Sky -> Picture
sky Sky {skyColorMain=cl, skySize=sz} =
  color cl . uncurry rectangleSolid $ sz

stdSky = Sky {
  skyColorMain = makeColorI 129 212 250 255,
  skySize = (800,800)
}