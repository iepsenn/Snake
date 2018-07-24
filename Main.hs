module Main(main) where

import Game
import Animation
import Rendering
import KeyHandler
import Graphics.Gloss.Data.ViewPort
import Graphics.Gloss

main :: IO () 
main = play window background fps initialState render handleKeys update 