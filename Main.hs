module Main(main) where

import Game
import Animation
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss



main :: IO ()
main = animate window background frame
	where 
		frame :: Float -> Picture
		frame seconds = render $ moveSnake seconds initialState
