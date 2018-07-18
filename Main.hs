module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort

width, height, offset :: Int
width = 300
height = 300
offset = 100

window :: Display
window = InWindow "Snake" (width, height) (offset, offset)

background :: Color
background = black

drawing :: Picture
drawing = pictures [food, snake 100, walls]
    where
      --food
      food = translate (-10) 40 $ color foodColor $ circleSolid 3
      foodColor = green
      --Snake
      snake :: Float -> Picture
      snake offset =
          translate 10 offset $
              color snakeColor $
                  rectangleSolid 10 10

      snakeColor = white

      -- walls from top and bottom
      wall :: Float -> Picture
      wall offset =
          translate 0 offset $
            color wallColor $
                rectangleSolid 300 20
      wallColor = white
      -- wals from sides
      wallSide :: Color -> Float -> Float -> Picture
      wallSide col x y = pictures[
                translate x y $ color col $ rectangleSolid 20 300,
                translate x y $ color col $ rectangleSolid 20 300
              ]


      walls = pictures [ wall 150, wall (-150), wallSide wallColor 150 0, wallSide wallColor (-150) 0]

main :: IO ()
main = display window background drawing
