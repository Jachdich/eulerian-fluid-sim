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
cellWidth = 1
cellHeight = 1

g = -9.81 -- downwards

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
            forM_ [0..(xCells*yCells)-1] $ \i -> do
                cell <- readArray (cells model) i
                let x = i `mod` xCells
                let y = i `div` xCells
                color $ Color3 (density cell) (density cell) (if wall cell then 1.0 else 0.0)
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

update :: Float -> Model -> IO Model
update dt model = do 
        -- putStrLn (show dt)
        return model --divergence dt model

-- integration dt model = model `withCells` [(Cell (value + g * dt) x y wall pressure) | (Cell value x y wall pressure) <- cells model]

divergence :: Float -> Model -> IO Model
divergence dt model = do 
                forM_ [0..xCells-1] $ \x -> do
                    forM_ [0..yCells-1] $ \y -> do
                        uxy <- uAt model x y
                        vxy <- vAt model x y
                        ux1y <- uAt model (x + 1) y
                        vxy1 <- vAt model x (y + 1)
                        let diverge = (uxy + vxy - ux1y - vxy1) / 4 * 1.9 -- 1.9 is over relaxation constant
                        let s = sum $ map fromEnum [isWall (x - 1) y, isWall x (y - 1), isWall (x + 1) y, isWall x (y + 1)]
                        uSet model x   y     (uxy  + diverge)
                        uSet model (x+1) y   (ux1y - diverge)
                        vSet model x   y     (vxy  - diverge)
                        vSet model x   (y+1) (vxy1 + diverge)
                        (Cell value wall pressure) <- readArray (cells model) (y * xCells + x)
                        return ()
                return model
            where
                isWall x y = False -- wall (cells model !! (y * xCells + x))
