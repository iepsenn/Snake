module Game where
import Graphics.Gloss

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
