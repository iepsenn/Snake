module Animation where

import Game
import Graphics.Gloss.Data.ViewPort


-- | Update the ball position using its current velocity.
moveSnake :: Float    -- ^ The number of seconds since last update
         -> GameState -- ^ The initial game state
         -> GameState -- ^ A new game state with an updated ball position
moveSnake seconds game = game { snakeLoc = (x', y') }
  where
    -- Old locations and velocities.
    (x, y) = snakeLoc game
    (vx, vy) = snakeVel game

    -- New locations.
    x' = x + vx * seconds
    y' = y --  + vy * seconds
