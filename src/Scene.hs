module Scene (
  Element (..),
  Sun(..),
  -- Kind (..),
  
  render,

  -- Constructor Functions
  sun,

  -- Standard Elements
  stdCloud,
  stdSun
  ) where

import Data.Fixed
import Graphics.Gloss

import Cloud
-- import Snow
import Sun
import Utils


data Element = SunElement Sun
             | CloudElement Cloud


render :: Float -> Element -> Picture
render t (CloudElement c) = cloud c{cloudTime=t}
render t (SunElement s) = sun s{sunTime=t}