import Graphics.Gloss
import System.Random
import System.IO.Unsafe

type Cell = (Float, Int, Int)
type Model = ([Cell], [Float], [Float])

horizontals :: Model -> [Float]
horizontals (cells, horiz, vert) = horiz

verticals :: Model -> [Float]
verticals (cells, horiz, vert) = vert

cells :: Model -> [Cell]
cells (cells, horiz, vert) = cells

width = 1280
height = 720
cellWidth :: Int
cellHeight :: Int
cellWidth = 100
cellHeight = 100

initial = ([(0.0, x, y) | x <- [1..(width `div` cellWidth)], y <- [1..(height `div` cellHeight)]], [], [])

main :: IO()
main = simulate (InWindow "Window" window (0, 0)) black 30 initial draw update
    where
        window = (width, height)


pos :: Int -> Int -> Picture -> Picture
pos x y = translate ((fromIntegral x * fromIntegral cellWidth) - (fromIntegral width / 2)) ((fromIntegral y * fromIntegral cellHeight) - (fromIntegral height / 2))

draw :: Model -> Picture
draw model = Pictures [color (makeColor col col col 1.0) $ pos x y $ rectangleSolid (fromIntegral cellWidth) (fromIntegral cellHeight) | (col, x, y) <- cells model]
update vp dt model = initial
