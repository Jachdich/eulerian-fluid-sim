import Graphics.Gloss
import System.Random
import System.IO.Unsafe
import Data.Array.IO

type Cell = (Float, Int, Int)
type FlowVec = IO (IOArray Int Int)
type Model = ([Cell], FlowVec, FlowVec)

horizontals :: Model -> FlowVec
horizontals (cells, horiz, vert) = horiz

verticals :: Model -> FlowVec
verticals (cells, horiz, vert) = vert

cells :: Model -> [Cell]
cells (cells, horiz, vert) = cells

withCells :: Model -> [Cell] -> Model
withCells (_, horiz, vert) cells = (cells, horiz, vert)

withHoriz :: Model -> FlowVec -> Model
withHoriz (cells, _, vert) horiz = (cells, horiz, vert)

withVert :: Model -> FlowVec -> Model
withVert (cells, horiz, _) vert = (cells, horiz, vert)

-- uAt :: Model -> Int -> Int -> Float
-- uAt model x y = (horizontals model) !! (y * xCells + x)
-- vAt :: Model -> Int -> Int -> Float
-- vAt model x y = (verticals model) !! (y * xCells + x)

width = 1280
height = 720
cellWidth :: Int
cellHeight :: Int
cellWidth = 100
cellHeight = 100

g = -9.81 -- downwards

xCells = width `div` cellWidth
yCells = height `div` cellHeight

initial :: Model
initial = ([(0.0, x, y) | x <- [1..(xCells)], y <- [1..(yCells)]],
           newArray (1,10) 37,
           newArray (1,10) 37)

main :: IO()
main = simulate (InWindow "Window" window (0, 0)) black 30 initial draw update
    where
        window = (width, height)


pos :: Int -> Int -> Picture -> Picture
pos x y = translate ((fromIntegral x * fromIntegral cellWidth) - (fromIntegral width / 2)) ((fromIntegral y * fromIntegral cellHeight) - (fromIntegral height / 2))

draw :: Model -> Picture
draw model = Pictures [color (makeColor col col col 1.0) $ pos x y $ rectangleSolid (fromIntegral cellWidth) (fromIntegral cellHeight) | (col, x, y) <- cells model]


update vp dt model = initial

-- integration dt model = model `withCells` [(value + g * dt, x, y) | (value, x, y) <- cells model]
-- divergence dt model = [
--                         (uAt model x y) + (divergence x y) / 4
--                         (uAt model (x+1) y) - (divergence x y) / 4
--                         (vAt model x y) - (divergence x y) / 4
--                         (vAt model x (y+1) + (divergence x y) / 4
--                          | x <- [0..xCells-1], y <- [0..yCells - 1]]
 
--     where
--         divergence x y = (uAt model x y) + vAt model x y
--                         -(uAt model (x + 1) y) - vAt model x (y + 1)