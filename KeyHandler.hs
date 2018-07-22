module KeyHandler where

import Game
import Graphics.Gloss.Interface.Pure.Game


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
