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


render :: Element -> Picture
render (CloudElement c) = cloud c
render (SunElement s) = sun s