import System.Random
import System.IO.Unsafe
import Data.Array.IO
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT 
import Codec.Picture
import Control.Monad (forM_, forM, replicateM_)

data Model = Model {wall :: IOArray Int Float,
                    horiz :: IOArray Int Float,
                    vert :: IOArray Int Float,
                    pressure :: IOArray Int Float,
                    newHoriz :: IOArray Int Float,
                    newVert :: IOArray Int Float}

at :: IOArray Int Float -> Int -> Int -> IO Float
at vec x y = readArray vec (x * yCells + y)
set :: IOArray Int Float -> Int -> Int -> Float -> IO ()
set vec x y = writeArray vec (x * yCells + y)
add :: IOArray Int Float -> Int -> Int -> Float -> IO ()
add vec x y val = do
    oldval <- at vec x y
    set vec x y (oldval + val)

width = 1280
height = 720
cellWidth :: Int
cellHeight :: Int
cellWidth = 10
cellHeight = 10
cellSpacing = 0.01

g = -9.81 -- downwards
dt = 1.0/60.0
fluidDensity = 1000.0 -- water ig

xCells = width `div` cellWidth
yCells = height `div` cellHeight

initial :: IO Model
initial = do
        horiz <- newArray (0,(xCells * (yCells + 1))) 0.0
        vert <- newArray (0,((xCells + 1) * yCells)) 0.0
        newHoriz <- newArray (0,(xCells * (yCells + 1))) 0.0
        newVert <- newArray (0,((xCells + 1) * yCells)) 0.0
        wall <- newArray (0,(xCells * yCells)) 0.0
        pressure <- newArray (0,(xCells * yCells)) 0.0
        forM_ [0..xCells-1] $ \x -> do
            forM_ [0..yCells-1] $ \y -> do
                set wall x y (isWall x y)
        return (Model wall horiz vert pressure newHoriz newVert)
    where
        isWall x y = if (x == 0 || y == 0 || x == xCells - 1 || y == yCells - 1) then 0.0 else 1.0

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

draw :: IORef Model -> DisplayCallback
draw state = do 
    clear [ColorBuffer]
    s <- get state
    drawModel s
    swapBuffers

idle :: IORef Model -> IdleCallback
idle state = do
  model <- get state
  postRedisplay Nothing

map_lol :: Float -> Float -> Float -> Float -> Float -> Float
map_lol a mi ma mi_ ma_ = (a - mi) / (ma - mi) * (ma_ - mi_) + mi_

drawModel :: Model -> IO ()
drawModel model = renderPrimitive Quads $ do
            update model
            pressures <- getElems (pressure model)
            let minPressure = minimum pressures
            let maxPressure = maximum pressures
            forM_ [0..(xCells*yCells)-1] $ \i -> do
                let x = i `mod` xCells
                let y = i `div` xCells
                pressure <- at (pressure model) x y
                wall <- at (wall model) x y
                let p = map_lol pressure minPressure maxPressure 0.0 1.0
                color $ Color3 p p (1.0 - wall)
                v <- readArray (vert model) i
                -- print v
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
        integrate model
        forM_ [0..xCells * yCells - 1] $ \i -> do
            writeArray (pressure model) i 0
        divergence model
        return model

integrate :: Model -> IO ()
integrate Model {wall, horiz, vert, pressure, newHoriz, newVert} = do
    forM_ [1..xCells-1] $ \x -> do
        forM_ [1..yCells-2] $ \y -> do
            wall0 <- at wall x y
            wall1 <- at wall x (y - 1)
            if wall0 /= 0.0 && wall1 /= 0.0 then
                add vert x y (g * dt)
            else return ()

divergence :: Model -> IO ()
divergence Model {wall, horiz, vert, pressure, newHoriz, newVert} = 
    replicateM_ 10 $
    forM_ [1..xCells-2] $ \x -> do
        forM_ [1..yCells-2] $ \y -> do
            x0 <- at wall (x - 1) y
            x1 <- at wall (x + 1) y
            y0 <- at wall x (y - 1)
            y1 <- at wall x (y + 1)
            let s = x0 + x1 + y0 + y1
            uxy <-  at horiz x y
            vxy <-  at vert  x y
            ux1y <- at horiz (x + 1) y
            vxy1 <- at vert  x (y + 1)
            let diverge = (ux1y - uxy + vxy1 - vxy)
            let p = -diverge / s * 1.9
            add pressure x y (p * coefficientPressure)
            add horiz  x     y    (- (p * x0))
            add horiz (x+1)  y    (  (p * x1))
            add vert   x     y    (- (p * y0))
            add vert   x    (y+1) (  (p * y1))
            return ()

    where
        coefficientPressure = fluidDensity * cellSpacing / dt

-- vadd :: Num a => (a, a) -> (a, a) -> (a, a)
-- (x0, y0) `vadd` (x1, y1) = (x0 + x1, y0 + y1)

-- vmul :: Num a => (a, a) -> (a, a) -> (a, a)
-- (x0, y0) `vmul` (x1, y1) = (x0 * x1, y0 * y1)

-- advection :: Model -> IO ()
-- advection Model {wall, horiz, vert, pressure, newHoriz, newVert} = do
--     forM_ [0..xCells * yCells - 1] $ \i -> do
--         u <- readArray horiz i
--         v <- readArray vert i
--         writeArray newHoriz i u
--         writeArray newVert i v
    
--     forM_ [1..xCells-2] $ \x -> do
--         forM_ [1..yCells-2] $ \y -> do
--             let currentX = x * cellSpacing
--             let currentY = y * cellSpacing + (cellSpacing / 2)
--             currentVec <- velocityAtU model x y
--             let previousPos = 1
--             return ()
--     where
--         velocityAtU model x y = do
--             u <- at (horiz model) x y
--             v01 <- at (vert model) (x-1) (y+1)
--             v11 <- at (vert model) x (y+1)
--             v00 <- at (vert model) (x-1) y
--             v10 <- at (vert model) x y
--             let v = (v01 + v11 + v00 + v10) / 4
--             return (u, v)
--         velocityAtV model x y = do
--             v <- at (vert model) x y
--             u0 <- at (horiz model) (x+1) (y-1)
--             u1 <- at (horiz model) x (y-1)
--             u2 <- at (horiz model) (x+1) y
--             u3 <- at (horiz model) x y
--             let v = (u0 + u1 + u2 + u3) / 4
--             return (u, v)
