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
    (x',y') = case moveDir game of
      W -> (x, y+vy*seconds)
      S -> (x, y-vy*seconds)
      A -> (x-vx*seconds, y)
      D -> (x+vx*seconds, y)