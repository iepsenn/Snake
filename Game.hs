module Game where
import Graphics.Gloss

data Direction = W | S | A | D
 deriving (Eq, Show)

data Pause = Yes | No
 deriving (Eq, Show)

data GameState = Game
  { snakeLoc :: [(Float, Float)]  -- ^  snake (x, y) location.
  , snakeVel :: (Float, Float)  -- ^  snake (x, y) velocity. 
  , snakeDir :: Direction
  , snakeRan :: Int
  , foodLoc :: (Float, Float) -- Location of the food
  , isPaused :: Pause
  , score :: Int
  } deriving Show 



-- | The starting state for the game of Snake.
initialState :: GameState
initialState = Game
  { snakeLoc = (0,0):(1,0) :(2,0):(3,0):(4,0):(5,0) : []
  , snakeVel = (50, 50)
  , snakeDir = D
  , snakeRan = 1
  , foodLoc = (50,50)
  , isPaused = No
  , score = 0
  }

  
window :: Display
window = InWindow "Snake comes alive!" (width, height) (offset, offset)

background :: Color
background = black


fps :: Int
fps = 60


width, height, offset :: Int
width = 300
height = 300
offset = 100

