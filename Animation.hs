module Animation where

import Game
import System.Random


type Radius = Float 
type Position = (Float, Float)



-- | Update the snake position using its current velocity.
movesnake :: Float    -- ^ The number of seconds since last update
         -> GameState -- ^ The initial game state
         -> GameState -- ^ A new game state with an updated snake position
movesnake seconds game = game { snakeLoc = moveFun (snakeLoc game) (snakeDir game) (seconds) (50)}


-- | Moves the snake around according to direction
moveFun :: [(Float,Float)] -> Direction -> Float -> Float -> [(Float,Float)]
moveFun [] _ _ _= []
moveFun ((x,y):xs) dir seconds vx = case dir of
      W -> (x, y+vx*seconds) : followFun (x,y) xs 
      S -> (x, y-vx*seconds) : followFun (x,y) xs 
      A -> (x-vx*seconds, y) : followFun (x,y) xs 
      D -> (x+vx*seconds, y) : followFun (x,y) xs 

-- | sub func makes tail follow head of snake
followFun :: (Float,Float) -> [(Float,Float)] -> [(Float,Float)]
followFun _ [] = []
followFun leader (follow:xs) = leader : followFun follow xs



posCheck :: GameState -> GameState
posCheck game = checkPosition (snakeLoc game) game 


-- | Checks Position for Snake or Food
checkPosition :: [(Float,Float)] -> GameState -> GameState
checkPosition [] game = game
checkPosition snake game = game { snakeLoc = newSnake, foodLoc = newFoodLoc, snakeRan = newSeed, score = newScore }
 where
  cond = isFood game (head snake) 4
  cond2 = isSnake (tail snake) (head snake) 
  (x,y) = head snake 

  newSnake = if cond
   then snakeLoc game ++ [(x,y)] ++ [(x,y)] ++ [(x,y)]++ [(x,y)]++ [(x,y)]++ [(x,y)]
     else if cond2
      then (0,0):(1,0) :(2,0):(3,0):(4,0):(5,0) : []
        else snakeLoc game

  b = drop (snakeRan game) (randomRs((-140),140) (mkStdGen 42) :: [Float])
  newSeed = snakeRan game + 1
  f_pos = take 2 b

  newFoodLoc = if cond then list2tup f_pos else foodLoc game

  newScore =
    if cond 
      then score game + 1
       else if cond2 then 0
        else score game
        
        
wallBounce :: GameState -> GameState
wallBounce game = game { snakeLoc = checkCollision (snakeLoc game) }


wallCollision :: Position -> Radius -> (Bool,Bool) 
wallCollision (x , y) radius = (s_wall1Col || s_wall2Col , topCollision || bottomCollision)
  where
    topCollision    = y - radius <= -fromIntegral width / 2 
    bottomCollision = y + radius >=  fromIntegral width / 2
    s_wall1Col = x - radius <= -fromIntegral height / 2
    s_wall2Col = x + radius >= fromIntegral height / 2


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



isFood :: GameState -> Position -> Radius-> Bool
isFood game (x,y) rad = (inXrange && inYrange)
  where
    (a,b) = foodLoc game
    temp = (a - x)
    temp2 = (b - y)
    disX = 
      if temp < 0 then (-temp) else temp
    disY = 
      if temp2 < 0 then (-temp2) else temp2

    inXrange = disX <= rad
    inYrange = disY <= rad


isSnake :: [Position] -> Position -> Bool
isSnake [] pos = False
isSnake (x:xs) pos = if (pos == x) then True else isSnake xs pos

list2tup :: [Float] -> (Float,Float)
list2tup [] = (0,0)
list2tup list = (head list, last list)



-- | Update the game by moving the snake and bouncing off walls.
update :: Float -> GameState -> GameState
update seconds game
      | isPaused game == Yes = game 
      | otherwise = posCheck(wallBounce(movesnake seconds game))