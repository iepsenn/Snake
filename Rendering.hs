module Rendering where

import Game
import Graphics.Gloss
import System.Random
-- | Convert a game state into a picture.
render :: GameState  -- ^ The game state to render.
       -> Picture   -- ^ A picture of this game state.
render game =
  pictures ([walls,food] ++ (drawSnake (snakeLoc game)))
  where
    --  The snake.
    drawSnake :: [(Float,Float)] -> [Picture]
    drawSnake [] = []
    drawSnake ((x,y):xs) = (translate x y $ color snakeColor $ rectangleSolid 10 10) : drawSnake xs
    snakeColor = dark red

    -- The food
    b = drop (snakeRan game) (randomRs((-150),150) (mkStdGen 42) :: [Float])
    f_pos = take 2 b


    food = translate (head f_pos) (last f_pos) $ color foodColor $ circleSolid 3
    foodColor = green

    --  The bottom and top walls.
    wall :: Float -> Picture
    wall offset =
      translate 0 offset $
        color wallColor $
          rectangleSolid 300 10

    s_wall :: Float -> Picture
    s_wall offset =
      translate offset 0 $
        color wallColor $
          rectangleSolid 10 310

    wallColor = greyN 0.5
    walls = pictures [wall 150, wall (-150), s_wall 150, s_wall (-150)]
