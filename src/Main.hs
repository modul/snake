{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss
import Debug.Trace
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

import Data.Function ((&))
import Data.Semigroup

type Time = Float

data Game = Game {
                age :: Time,
                points :: Int,
                state :: GameState,
                snake :: Snake,
                speed :: Float, 
                lastDraw :: Time,
                changeHead :: [Heading]
            }
data Snake = Snake {                
                shape :: Path,
                heading :: Heading
                }

data GameState = Running | Paused | GameOver

data Pixel = Pixel Point Heading

data Heading = North | East | South | West

instance Semigroup Heading where
    North <> South = North
    East <> West = East
    South <> North = South
    West <> East = West
    _ <> b = b

pixelsize = 20

initSnake = Snake [(-2, 0), (-1, 0), (0, 0)] East 
initGame = Game 0 0 Running initSnake 5 0 []

turn :: Snake -> Heading -> Snake
turn s@Snake{..} direction = s {heading = heading <> direction}

render :: Game -> Picture
render Game{..} = drawSnake snake

drawSnake Snake{..} = map drawPixel shape & pictures

pixelShape = rectangleSolid pixelsize pixelsize
drawPixel (x, y) = pixelShape & translate x y & color white 

handle :: Event -> Game -> Game
-- handle (EventKey (SpecialKey KeyUp    ) Down _ _) g@Game{..} = g {snake = turn snake North}
-- handle (EventKey (SpecialKey KeyRight ) Down _ _) g@Game{..} = g {snake = turn snake East }
-- handle (EventKey (SpecialKey KeyDown  ) Down _ _) g@Game{..} = g {snake = turn snake South}
-- handle (EventKey (SpecialKey KeyLeft  ) Down _ _) g@Game{..} = g {snake = turn snake West }
handle (EventKey (SpecialKey KeyUp    ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [North]}
handle (EventKey (SpecialKey KeyRight ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [East] }
handle (EventKey (SpecialKey KeyDown  ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [South]}
handle (EventKey (SpecialKey KeyLeft  ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [West] }
handle _ game = game

update :: Time -> Game -> Game
update dt g@Game{..} = g {snake = refresh . newHeading $ snake,
                          age = age + dt, 
                          lastDraw = if doUpdate then age else lastDraw,
                          changeHead = if doUpdate && not (null changeHead) then tail changeHead else changeHead
                          }
    where refresh x  = if doUpdate then updateSnake x else x
          newHeading x = if doUpdate && not (null changeHead) then turn x (head changeHead) else x
          doUpdate = lastDraw + period < age
          period = 1 / speed

updateSnake :: Snake -> Snake
updateSnake s@Snake{..}= s {shape = shape'}
    where shape' = movePixel heading hd : init shape           
          hd = head shape

move :: Heading -> Float -> Point -> Point
move North dist (x, y) = (x, y + dist)
move East  dist (x, y) = (x + dist, y)
move West  dist (x, y) = (x - dist, y)
move South dist (x, y) = (x, y - dist)

movePixel :: Heading -> Point -> Point
movePixel h = move h pixelsize

main :: IO ()
main = do
    play FullScreen bg fps game render handle update
    where bg = black
          fps = 60
          game = initGame


