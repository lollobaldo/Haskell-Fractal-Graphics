module Main where

import Graphics.Gloss
import Sun

main :: IO ()
main = do
  putStrLn "hello world"
  animate window background frame
  where
    frame :: Float -> Picture
    frame s = sun $ stdSun {time=s}

width, height, offset :: Int
width = 800
height = 800
offset = 50

window :: Display
window = InWindow "Fractals" (width, height) (offset, offset)

background :: Color
background = white

stdSun = Sun {
  colorStart = red,
  colorEnd = yellow,
  coords = (-200, 300),
  period = 3,
  size = 100,
  steps = 15,
  time = 0
}