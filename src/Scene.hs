module Scene (
  Element (..),
  -- Sun(..),
  -- Kind (..),
  
  render,
  -- Standard Elements
  cloudElem,
  skyElem,
  sunElem,

  -- | Element attributes
  cloudKind,
  cloudColorMain,
  cloudCoords,
  cloudRans,
  cloudSize',
  cloudSteps,
  -- | SKY
  skyColorMain,
  skySize,
  -- | SUN
  sunColorStart,
  sunColorEnd,
  sunCoords,
  sunPeriod,
  sunSize,
  sunSteps
  ) where

import Data.Fixed
import Graphics.Gloss

import Cloud
import Sky
import Sun
-- import Utils


data Element = CloudElement Cloud
             | SkyElement Sky
             | SunElement Sun


render :: Float -> Element -> Picture
render t (CloudElement c) = cloud c{cloudTime=t}
render t (SkyElement s) = sky s--{skyTime=t}
render t (SunElement s) = sun s{sunTime=t}



cloudElem, skyElem, sunElem :: Element
cloudElem = CloudElement stdCloud
skyElem = SkyElement stdSky
sunElem = SunElement stdSun