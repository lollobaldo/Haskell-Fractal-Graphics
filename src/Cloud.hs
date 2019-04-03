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
  cloudKind :: CloudKind,
  cloudColorMain :: Color,
  cloudCoords :: Coords,
  cloudRans :: [Float],
  cloudSize' :: Size',
  cloudSteps :: Step,
  cloudTime :: Float
}

cloud :: Cloud -> Picture
cloud Cloud {cloudKind=k, cloudColorMain=cl, cloudCoords=(x,y), cloudRans=r,
             cloudSize'=sz@(w,h), cloudSteps=st, cloudTime=t} =
    uncurry translate c' . color cl . uncurry rectangleSolid $ sz
  where
    c' = (x+w/2, y+h/2)

stdCloud = Cloud {
  cloudKind = None,
  cloudColorMain = greyN 0.5,
  cloudCoords = (200,300),
  cloudRans = [],
  cloudSize' = (150,50),
  cloudSteps = 3,
  cloudTime = 0
}