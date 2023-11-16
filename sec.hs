import System.Random
import System.IO.Unsafe
import Data.Array.IO
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT 
import Codec.Picture
import Control.Monad (forM_, forM)

data Cell = Cell {density :: Float, wall :: Bool, pressure :: Float}
type FlowVec = (IOArray Int Float)
data Model = Model {cells :: IOArray Int Cell, horiz :: FlowVec, vert :: FlowVec}

uAt :: Model -> Int -> Int -> IO Float
uAt model x y = readArray (horiz model) (y * xCells + x)
vAt :: Model -> Int -> Int -> IO Float
vAt model x y = readArray (vert model) (y * xCells + x)

uSet :: Model -> Int -> Int -> Float -> IO ()
uSet model x y = writeArray (horiz model) (y * xCells + x)
vSet :: Model -> Int -> Int -> Float -> IO ()
vSet model x y = writeArray (vert model) (y * xCells + x)

width = 1280
height = 720
cellWidth :: Int
cellHeight :: Int
cellWidth = 10
cellHeight = 10
cellSpacing = (fromIntegral cellWidth) * 0.01 -- assume 1 pixel = 0.01m

g = -9.81 -- downwards
dt = 1.0/30.0
fluidDensity = 1000.0 -- water ig

xCells = width `div` cellWidth
yCells = height `div` cellHeight

initial :: IO Model
initial = do
        horiz <- newArray (0,(xCells * (yCells + 1))) 0.0
        vert <- newArray (0,((xCells + 1) * yCells)) 0.0
        cells <- newArray (0,(xCells * yCells)) (Cell 1.0 False 0.0)
        forM_ [0..xCells-1] $ \x -> do
            forM_ [0..yCells-1] $ \y -> do
                writeArray cells (y * xCells + x) (Cell 1.0 (isWall x y) 0.0)
        return (Model cells horiz vert)
        -- return ([(Cell 1.0 x y (isWall x y) 0.0) | x <- [0..(xCells-1)], y <- [0..(yCells-1)]], horiz, vert)
    where
        isWall x y = x == 0 || y == 0 || x == xCells - 1 || y == yCells - 1

main :: IO ()
main = do
    model <- initial
    getArgsAndInitialize
    initialDisplayMode $= [SingleBuffered, RGBMode]
    initialWindowSize $= Size (fromIntegral width) (fromIntegral height)
    state <- newIORef model
    createWindow "Test"
    displayCallback $= draw state
    idleCallback $= Just (idle state)
    mainLoop

-- pos :: Int -> Int -> Picture -> Picture
-- pos x y = translate ((fromIntegral x * fromIntegral cellWidth) - (fromIntegral width / 2)) (fromIntegral (y * cellHeight) - (fromIntegral height / 2))
-- rectangleSolidCorner width height = translate (width / 2) (height / 2) $ rectangleSolid width height


draw :: IORef Model -> DisplayCallback
draw state = do 
    clear [ColorBuffer]
    s <- get state
    drawModel s
    swapBuffers

idle :: IORef Model -> IdleCallback
idle state = do
  -- state $~! (+ 1)
  postRedisplay Nothing


drawModel :: Model -> IO ()
drawModel model = renderPrimitive Quads $ do
            putStrLn "he"
            update model
            forM_ [0..(xCells*yCells)-1] $ \i -> do
                cell <- readArray (cells model) i
                let x = i `mod` xCells
                let y = i `div` xCells
                color $ Color3 (pressure cell) (pressure cell) (if wall cell then 1.0 else 0.0)
                print $ pressure cell
                let screenX = (fromIntegral x / fromIntegral xCells * 2 - 1) :: GLfloat
                let screenY = (fromIntegral y / fromIntegral yCells * 2 - 1) :: GLfloat
                -- print screenX
                let dx = (fromIntegral cellWidth) / (fromIntegral width) * 2.0
                let dy = (fromIntegral cellHeight) / (fromIntegral height) * 2.0
                vertex $ Vertex2 screenX screenY
                vertex $ Vertex2 (screenX + dx) screenY
                vertex $ Vertex2 (screenX + dx) (screenY + dy)
                vertex $ Vertex2 screenX (screenY + dy)

-- handleInput :: Event -> Model -> IO Model
-- handleInput event model = do return model

update :: Model -> IO Model
update model = do 
        -- integrate model
        mapArray (\(Cell a b pressure) -> Cell a b 0) (cells model)
        model <- divergence model
        return model


integrate model = do
    forM_ [1..xCells-1] $ \x -> do
        forM_ [1..yCells-1] $ \y -> do
            (Cell _ wall _) <- readArray (cells model) (y * xCells + x)
            (Cell _ wall1 _) <- readArray (cells model) (y * xCells + x - 1)
            v <- vAt model x y
            if not wall && not wall1 then
                vSet model x y (v + g * dt)
            else return ()

divergence :: Model -> IO Model
divergence model = do
                let coefficientPressure = fluidDensity * cellSpacing / dt
                forM_ [1..xCells-2] $ \x -> do
                    forM_ [1..yCells-2] $ \y -> do
                        w1 <- isWall (x - 1) y
                        w2 <- isWall x (y - 1)
                        w3 <- isWall (x + 1) y
                        w4 <- isWall x (y + 1)
                        let s = sum $ map fromEnum $ map not [w1, w2, w3, w4]
                        uxy <- uAt model x y
                        vxy <- vAt model x y
                        ux1y <- uAt model (x + 1) y
                        vxy1 <- vAt model x (y + 1)
                        let diverge = (uxy + vxy - ux1y - vxy1) / 4
                        cell <- readArray (cells model) (y * xCells + x)
                        let p = -diverge / (fromIntegral s)-- * 1.9; -- maybe -diverge
                        let pressureHere = (pressure cell) + p * coefficientPressure
                        uSet model x   y     (uxy  - (p * fromIntegral $ fromEnum $ not w1))
                        uSet model (x+1) y   (ux1y + (p * fromIntegral $ fromEnum $ not w3))
                        vSet model x   y     (vxy  - (p * fromIntegral $ fromEnum $ not w2))
                        vSet model x   (y+1) (vxy1 + (p * fromIntegral $ fromEnum $ not w4))
                        writeArray (cells model) (y * xCells + x) (Cell (density cell) (wall cell) pressureHere)
                        return ()
                return model
            where
                isWall x y = do
                    (Cell _ wall _) <- readArray (cells model) (y * xCells + x)
                    return wall

-- advection :: Model -> IO Model
-- advection model = forM_ [1..xCells-2] $ \x -> do
--                       forM_ [1..yCells-2] $ \y -> do
--                           let currentPos = ...
--                           currentVec <- uAt model x y
--                           let previousPos = ...
                          