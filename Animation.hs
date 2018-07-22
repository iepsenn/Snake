module Animation where

import Game

-- | Update the snake position using its current velocity.
movesnake :: Float    -- ^ The number of seconds since last update
         -> GameState -- ^ The initial game state
         -> GameState -- ^ A new game state with an updated snake position
movesnake seconds game = game { snakeLoc = moveFun (snakeLoc game) (snakeDir game) (seconds) (50)}
    -- Old locations and velocities.
    --(x, y) = snakeLoc game
    --(vx, vy) = snakeVel game
{-
    -- New locations.{-}
    (x',y') = case snakeDir game of
      W -> (x, y+vy*seconds)
      S -> (x, y-vy*seconds)
      A -> (x-vx*seconds, y)
      D -> (x+vx*seconds, y)
    --x' = x + vx * seconds
    --y' = y + vy * seconds-}
   -}

moveFun :: [(Float,Float)] -> Direction -> Float -> Float -> [(Float,Float)]
moveFun [] _ _ _= []
moveFun ((x,y):xs) dir seconds vx = case dir of
      W -> (x, y+vx*seconds) : followFun (x,y) xs 
      S -> (x, y-vx*seconds) : followFun (x,y) xs 
      A -> (x-vx*seconds, y) : followFun (x,y) xs 
      D -> (x+vx*seconds, y) : followFun (x,y) xs 

followFun :: (Float,Float) -> [(Float,Float)] -> [(Float,Float)]
followFun _ [] = []
followFun leader (follow:xs) = leader : followFun follow xs
{-}
moveFun :: [(Float,Float)] -> Direction -> Float -> Float -> [(Float,Float)]
moveFun [] _ _ _= []
moveFun ((x,y):xs) dir seconds vx = case dir of
      W -> (x, y+vx*seconds) : moveFun xs dir seconds vx
      S -> (x, y-vx*seconds) : moveFun xs dir seconds vx
      A -> (x-vx*seconds, y) : moveFun xs dir seconds vx
      D -> (x+vx*seconds, y) : moveFun xs dir seconds vx

-}

type Radius = Float 
type Position = (Float, Float)



wallCollision :: Position -> Radius -> (Bool,Bool) 
wallCollision (x , y) radius = (s_wall1Col || s_wall2Col , topCollision || bottomCollision)
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2
    s_wall1Col = x - radius <= -fromIntegral height / 2
    s_wall2Col = x + radius >= fromIntegral height / 2


wallBounce :: GameState -> GameState
wallBounce game = game { snakeLoc = checkCollision (snakeLoc game) }
 {-} where
    -- Radius. Use the same thing as in `render`.
    radius = 10

    -- The old velocities.

    (vx, vy) = snakeLoc game

    (vx',vy') = if (wallCollision (snakeLoc game) radius) == (False, True)
          then
             (vx,-vy)
           else if (wallCollision (snakeLoc game) radius) == (True, False)
            then
               (-vx,vy)
             else if (wallCollision (snakeLoc game) radius) == (True, True)
               then
                 (-vx,-vy)
                else
                 (vx,vy)-}

checkCollision :: [(Float,Float)] -> [(Float,Float)]
checkCollision [] = []
checkCollision ((x,y):xs) = 
  let cond = wallCollision (x,y) 10 in
  if cond == (False, True)
    then (x,-y) : checkCollision xs
     else if cond == (True, False)
       then (-x,y) : checkCollision xs
        else
          (x,y) : checkCollision xs

-- | Update the game by moving the snake and bouncing off walls.
update :: Float -> GameState -> GameState
update  seconds = wallBounce . movesnake seconds
