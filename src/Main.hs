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
type Dimensions = (Float, Float)

data Game = Game {
                age :: Time,
                points :: Int,
                state :: GameState,
                snake :: Snake,
                speed :: Float, 
                lastDraw :: Time,
                changeHead :: [Heading],
                food :: [Point],
                dimensions :: Dimensions
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
initGame dim = Game 0 0 Running initSnake 10 0 [] [(-80, -400)] dim

turn :: Snake -> Heading -> Snake
turn s@Snake{..} direction = s {heading = heading <> direction}

render :: Game -> Picture
render Game{..} = pictures $ drawState state : drawScore dimensions points : drawFood food : drawSnake snake 

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

drawScore (w, h) score = text (show score) 
                       & color (dim white)
                       & scale 0.5 0.5
                       & translate (-10) (-h/2 + 10)
                    

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
                                points = 10 * length (shape snake') - 30,
                                changeHead = if not (null changeHead) then tail changeHead else changeHead,
                                state = collision snake
                               } 
               | otherwise = x
          (snake', food') = eat food . warp dimensions $ snake

eat fs (Snake shape h) = (Snake shape' h,  food')
    where (shape', food') = if m == f 
                             then (m : shape, drop 1 fs)  
                             else (m : init shape, fs)
          m = movePixel h p
          p = head shape
          f = head fs

collision :: Snake -> GameState
collision (Snake (s:ss) h) = if hit then GameOver else Running
    where hit = m `elem` obst
          m = movePixel h s
          obst = s:ss

warp :: Dimensions -> Snake -> Snake
warp (dx, dy) s@Snake{..} = s {shape = (hx', hy'):tail shape}
    where (hx, hy) = head shape
          hx' = bound l r hx
          hy' = bound b t hy
          r = dx/2 
          t = dy/2
          l = -r
          b = -t

bound :: Float -> Float -> Float -> Float
bound min max val | val > max = min
                  | val < min = max
                  | otherwise = val

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

newGame rnd (w, h) = game {food = fs}
    where fs = randomCoords rnd pixelsize xlim ylim
          (xlim, ylim) = (lim w, lim h)
          lim n = let n' = n `div` 2 in (-n', n')
          game = initGame (fit w, fit h)
          fit = (*pixelsize) . fromIntegral . round . (/pixelsize) . fromIntegral

main :: IO ()
main = do
    g <- newStdGen
    dim <- getScreenSize
    let game = newGame g dim
    play FullScreen bg fps game render handle update
    where bg = black
          fps = 60

