{-# LANGUAGE RecordWildCards #-}

module Main where

import Graphics.Gloss
import Debug.Trace
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Interface.Environment

import Data.Function ((&))
import Data.Semigroup
import System.Random

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

data GameState = Running | Paused | GameOver deriving (Eq, Show)


data Heading = North | East | South | West

instance Semigroup Heading where
    North <> South = North
    East <> West = East
    South <> North = South
    West <> East = West
    _ <> b = b

pixelsize = 20

initSnake = Snake [(0, 0), (0, 0), (0, 0)] East 
initGame = Game 0 0 Running initSnake 10 0 [] [(-80, -400)]

turn :: Snake -> Heading -> Snake
turn s@Snake{..} direction = s {heading = heading <> direction}

render :: Game -> Picture
render Game{..} = pictures $ drawState state : drawFood food : drawSnake snake 

borderShape = rectangleWire pixelsize pixelsize
pixelShape = rectangleSolid pixelsize pixelsize
foodShape = circleSolid (pixelsize/2)

draw shape clr (x, y) = shape & translate x y & color clr
drawPixel = draw pixelShape white
drawBorder = draw borderShape (dim white)
drawFood = draw foodShape cyan . head
drawSnake Snake{..} = map drawPixel shape ++ map drawBorder shape

drawState Paused = text "Paused" & color red
drawState GameOver = text "Game Over" & color red
drawState _ = blank

handle :: Event -> Game -> Game
handle (EventKey (SpecialKey KeyUp    ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [North]}
handle (EventKey (SpecialKey KeyRight ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [East] }
handle (EventKey (SpecialKey KeyDown  ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [South]}
handle (EventKey (SpecialKey KeyLeft  ) Down _ _) g@Game{..} = g {changeHead = changeHead ++ [West] }
handle (EventKey (SpecialKey KeySpace ) Down _ _) g@Game{..} = g {state = case state of 
                                                                            Paused -> Running
                                                                            Running -> Paused
                                                                            otherwise -> state
                                                                 }
handle _ game = game

update :: Time -> Game -> Game
update dt g@Game{..} = up g {age = age + dt}
    where newHeading x = if not (null changeHead) then turn x (head changeHead) else x
          doUpdate = lastDraw + period < age && state == Running
          period = 1 / speed
          up x | doUpdate  = x {snake = newHeading snake',
                                food = food',
                               lastDraw = age,
                               changeHead = if not (null changeHead) then tail changeHead else changeHead
                               } 
               | otherwise = x
          (snake', food') = eat food snake

eat fs (Snake shape h) = (Snake shape' h,  food')
    where (shape', food') = if m == f 
                             then (m : shape, drop 1 fs)  
                             else (m : init shape, fs)
          m = movePixel h p
          p = head shape
          f = head fs

move :: Heading -> Float -> Point -> Point
move North dist (x, y) = (x, y + dist)
move East  dist (x, y) = (x + dist, y)
move West  dist (x, y) = (x - dist, y)
move South dist (x, y) = (x, y - dist)

movePixel :: Heading -> Point -> Point
movePixel h = move h pixelsize

randomCoords :: StdGen -> Float -> (Int, Int) -> (Int, Int) -> [Point]
randomCoords gen gridsize xbound ybound = zip xs ys
    where xs = grid xrange
          ys = grid yrange
          xrange = randomRs xbound gx
          yrange = randomRs ybound gy
          (gx, gy) = split gen
          grid = map fromIntegral . filter fitgrid
          fitgrid = (==0) . flip mod (round gridsize)

newGame rnd (w, h) = initGame {food = fs}
    where fs = randomCoords rnd pixelsize xlim ylim
          (xlim, ylim) = (lim w, lim h)
          lim n = let n' = n `div` 2 in (-n', n')

main :: IO ()
main = do
    g <- newStdGen
    dim <- getScreenSize
    let game = newGame g dim
    play FullScreen bg fps game render handle update
    where bg = black
          fps = 60

