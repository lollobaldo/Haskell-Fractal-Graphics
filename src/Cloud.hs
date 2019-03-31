module Cloud (
  Cloud(..),
  cloud,
  stdCloud) where

import Data.Fixed
import Graphics.Gloss
import Utils

data CloudKind = None
               | Rain
               | Snow

data Cloud = Cloud {
  elemKind :: CloudKind,
  elemColorMain :: Color,
  elemCoords :: Coords,
  elemRans :: [Float],
  elemSize' :: Size',
  elemSteps :: Step,
  elemTime :: Float
}

cloud :: Cloud -> Picture
cloud Cloud {elemKind=k, elemColorMain=c1, elemCoords=c, elemRans=r,
             elemSize'=sz, elemSteps=st, elemTime=t} =
    uncurry translate c . uncurry rectangleSolid $ c
cloud _ = error "Cannot initialise as Cloud"

stdCloud = Cloud {
  elemKind = None,
  elemColorMain = black,
  elemCoords = (200,300),
  elemRans = [],
  elemSize' = (150,50),
  elemSteps = 3,
  elemTime = 0
}