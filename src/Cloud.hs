module Cloud (
  cloud) where

import Data.Fixed
import Graphics.Gloss
import Utils

cloud :: Element -> Picture
cloud Cloud {elemKind=k, elemColorMain=c1, elemCoords=c, elemRans=r,
             elemSize'=sz, elemSteps=st, elemTime=t} =
    uncurry translate c . uncurry rectangleSolid $ c
cloud _ = error "Cannot initialise as Cloud"