module Game where
import Graphics.Gloss

data Direction = W | S | A | D
  deriving Show

data GameState = GameS
    { foodLoc :: (Float, Float)  -- Location of food
	, snakeLoc :: (Float, Float)  -- Snake Tail xy, Snake Head xy
	, snakeVel :: (Float, Float)   --snakes Velocity
	, moveDir :: Direction  -- The direction the snake is moving
	} deriving Show


initialState :: GameState
initialState = GameS
   { foodLoc = (50, 40)
   , snakeLoc = (-10,30)
   , snakeVel = (1,-3)
   , moveDir = D
   }

window :: Display
window = InWindow "Snake" (width, height) (offset, offset)

background :: Color
background = black

--F to render top/bottom walls
wall :: Float -> Picture
wall offset =
  translate 0 offset $
    color white $
        rectangleSolid 300 20


--F to render side walss
wallSide :: Color -> Float -> Float -> Picture
wallSide col x y = pictures[
        translate x y $ color col $ rectangleSolid 20 320,
        translate x y $ color col $ rectangleSolid 20 320
      ]



render :: GameState -> Picture
render game = pictures [food, walls, snake 100]
    where
    	-- Food
    	food :: Picture
    	food = uncurry translate (foodLoc game) $ color foodColor $ circleSolid 3
    	foodColor = green

    	--Snake
    	snake :: Float -> Picture
    	snake offset = uncurry translate (snakeLoc game) $ color snakeColor $ rectangleSolid 10 10
    	snakeColor = white

        --Walls
        walls :: Picture
        walls = pictures [ wall 150, wall (-150), wallSide wallColor 150 0, wallSide wallColor (-150) 0]
        wallColor = red






























width, height, offset :: Int
width = 300
height = 300
offset = 100





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