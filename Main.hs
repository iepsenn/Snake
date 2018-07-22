module Main(main) where

import Graphics.Gloss
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss.Interface.Pure.Game
import System.Random
-- GAME ####################################################################


data Direction = W | S | A | D
 deriving (Eq, Show)

data GameState = Game
  { snakeLoc :: [(Float, Float)]  -- ^ Pong snake (x, y) location.
  , snakeVel :: (Float, Float)  -- ^ Pong snake (x, y) velocity. 
  , snakeDir :: Direction
  , snakeRan :: Int
  } deriving Show 



-- | The starting state for the game of Pong.
initialState :: GameState
initialState = Game
  { snakeLoc = (0,0) : (10,0) :(20,0):(30,0):(40,0):(50,0) : []
  , snakeVel = (50, 50)
  , snakeDir = D
  , snakeRan = 1
  }


window :: Display
window = InWindow "myexample" (width, height) (offset, offset)

background :: Color
background = black


fps :: Int
fps = 60


width, height, offset :: Int
width = 300
height = 300
offset = 100


-- RENDERING ######################################################

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



-- ANIMATION   #######################################################

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






-- INPUT_HANDLER #####################################################

handleKeys :: Event -> GameState -> GameState
handleKeys (EventKey (Char 'w') _ _ _) game = game { snakeDir = x }
  where
    x = if snakeDir game /= S then W else snakeDir game
handleKeys (EventKey (Char 's') _ _ _) game = game { snakeDir = x }
  where
    x = if snakeDir game /= W then S else snakeDir game
handleKeys (EventKey (Char 'a') _ _ _) game = game { snakeDir = x }
  where
    x = if snakeDir game /= D then A else snakeDir game
handleKeys (EventKey (Char 'd') _ _ _) game = game { snakeDir = x }
  where
    x = if snakeDir game /= A then D else snakeDir game
handleKeys _ game = game








main :: IO ()
main = play window background fps initialState render handleKeys update
