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
                changeHead :: [Heading],
                food :: [Point]
            }
data Snake = Snake {                
                shape :: Path,
                heading :: Heading
                }

data GameState = Running | Paused | GameOver


data Heading = North | East | South | West

instance Semigroup Heading where
    North <> South = North
    East <> West = East
    South <> North = South
    West <> East = West
    _ <> b = b

pixelsize = 20

initSnake = Snake [(0, 0), (0, 0), (0, 0)] East 
initGame = Game 0 0 Running initSnake 5 0 [] [(-80, -400)]

turn :: Snake -> Heading -> Snake
turn s@Snake{..} direction = s {heading = heading <> direction}

render :: Game -> Picture
render Game{..} = pictures $ drawSnake snake ++ drawFood food


pixelShape = rectangleSolid pixelsize pixelsize
foodShape = circleSolid (pixelsize/2)

draw shape clr (x, y) = shape & translate x y & color clr
drawPixel = draw pixelShape white
drawFood = map (draw foodShape cyan)
drawSnake Snake{..} = map drawPixel shape 

handle :: Event -> Game -> Game
handle (EventKey (SpecialKey KeyUp    ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [North]}
handle (EventKey (SpecialKey KeyRight ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [East] }
handle (EventKey (SpecialKey KeyDown  ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [South]}
handle (EventKey (SpecialKey KeyLeft  ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [West] }
handle _ game = game

update :: Time -> Game -> Game
update dt g@Game{..} = trace (show . head . shape $ snake) $ up g {age = age + dt}
    where newHeading x = if not (null changeHead) then turn x (head changeHead) else x
          doUpdate = lastDraw + period < age
          period = 1 / speed
          up x | doUpdate  = x {snake = newHeading snake',
                                food = food',
                               lastDraw = age,
                               changeHead = if not (null changeHead) then tail changeHead else changeHead
                               } 
               | otherwise = x
          (snake', food') = eat food snake

eat fs (Snake shape h) = (Snake shape' h,  food')
    where (shape', food') = if m `elem` fs 
                             then (m : shape, (0, 0) : [f | f <- fs, f /= m])  
                             else (m : init shape, fs)
          m = movePixel h p
          p = head shape

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


