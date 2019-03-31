module Main where

import Graphics.Gloss
import Scene

main :: IO ()
main = do
  putStrLn "hello world"
  animate window background frame
  where
    frame :: Float -> Picture
    frame s = sun $ stdSun {elemTime=s}

width, height, offset :: Int
width = 800
height = 800
offset = 50

window :: Display
window = InWindow "Fractals" (width, height) (offset, offset)

background :: Color
background = white