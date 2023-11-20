import Data.Array.IO
import Data.IORef
import qualified Graphics.Rendering.OpenGL as GL
import Graphics.UI.GLUT
import Codec.Picture
-- import Data.Color
import Control.Monad (forM_, forM, replicateM_, when)
import GHC.Utils.Misc (uncurry3)

data Model = Model {wall :: IOUArray Int Float,
                    horiz :: IOUArray Int Float,
                    vert :: IOUArray Int Float,
                    pressure :: IOUArray Int Float,
                    newHoriz :: IOUArray Int Float,
                    newVert :: IOUArray Int Float,
                    smoke :: IOUArray Int Float,
                    newSmoke :: IOUArray Int Float}

at :: IOUArray Int Float -> Int -> Int -> IO Float
at vec x y = readArray vec (y * xCells + x)
set :: IOUArray Int Float -> Int -> Int -> Float -> IO ()
set vec x y = writeArray vec (y * xCells + x)
add :: IOUArray Int Float -> Int -> Int -> Float -> IO ()
add vec x y val = do
    oldval <- at vec x y
    set vec x y (oldval + val)

width = 1000
height = 720
cellWidth :: Int
cellHeight :: Int
cellWidth = 2
cellHeight = 2
cellSpacing = 1.0 / fromIntegral yCells
numIters = 40
drawingMode = Velocity

data DrawingMode = Smoke | Pressure | Velocity
    deriving (Eq)

g = -9.81
dt = 1.0/240.0
fluidDensity = 1000.0

xCells = width `div` cellWidth
yCells = height `div` cellHeight

initial :: IO Model
initial = do
        horiz <- newArray (0,xCells * (yCells + 1)) 0.0
        vert <- newArray (0,(xCells + 1) * yCells) 0.0
        newHoriz <- newArray (0,xCells * (yCells + 1)) 0.0
        newVert <- newArray (0,(xCells + 1) * yCells) 0.0
        wall <- newArray (0,xCells * yCells) 0.0
        pressure <- newArray (0,xCells * yCells) 0.0
        smoke <- newArray (0,xCells * yCells) 1.0
        newSmoke <- newArray (0,xCells * yCells) 1.0
        forM_ [0..xCells-1] $ \x -> do
            forM_ [0..yCells-1] $ \y -> do
                set wall x y (isWall x y)
        return (Model wall horiz vert pressure newHoriz newVert smoke newSmoke)
    where
        isWall x y = if x == 0 || y == 0 || y == yCells - 1 then 0.0 else 1.0

main :: IO ()
main = do
    model <- initial
    setObject model (width `div` 6) (height `div` 2)
    getArgsAndInitialize
    initialDisplayMode $= [SingleBuffered, RGBMode]
    initialWindowSize $= Size (fromIntegral width) (fromIntegral height)
    state <- newIORef model
    createWindow "Test"
    displayCallback $= draw state
    idleCallback $= Just (idle state)
    motionCallback $= Just (mouse state)
    mainLoop

mouse :: IORef Model -> Position -> IO ()
mouse state (Position x y) = do
    model <- get state
    setObject model (fromIntegral x) (fromIntegral y)

setObject :: Model -> Int -> Int -> IO ()
setObject Model {wall=wall, horiz=horiz, vert=vert, newHoriz=newHoriz, newVert=newVert} x y = do
    let normX = fromIntegral x / fromIntegral xCells * cellSpacing
    let normY = fromIntegral y / fromIntegral yCells * cellSpacing
    let vx = 0.0
    let vy = 0.0

    let r = fromIntegral cellWidth * fromIntegral (yCells `div` 12)
    forM_ [1..xCells - 3] $ \i ->
        forM_ [1..yCells - 3] $ \j -> do
            let dx = (fromIntegral i + 0.5) * fromIntegral cellWidth - fromIntegral x
            let dy = (fromIntegral j + 0.5) * fromIntegral cellWidth - fromIntegral (height - fromIntegral y)
            if dx * dx + dy * dy < r * r then do
                set horiz i j 0
                set horiz (i + 1) j 0
                set vert i j 0
                set vert i (j + 1) 0
                set newHoriz i j 0
                set newHoriz (i + 1) j 0
                set newVert i j 0
                set newVert i (j + 1) 0
                set wall i j 0.0
            else
                set wall i j 1.0

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

drawModel :: Model -> IO ()
drawModel model = renderPrimitive Quads $ do
            update model
            pressures <- getElems (pressure model)
            let minPressure = minimum pressures
            let maxPressure = maximum pressures
            smokes <- getElems (smoke model)
            let minSmoke = minimum smokes
            let maxSmoke = maximum smokes
            forM_ [0..(xCells*yCells)-1] $ \i -> do
                let x = i `mod` xCells
                let y = i `div` xCells
                pressure <- at (pressure model) x y
                smoke <- at (smoke model) x y
                let smokeNorm = 1 - (1 - smoke) * 2  
                wall <- at (wall model) x y
                u <- readArray (horiz model) i
                let a = if u < 0 then (-u) else 0
                let b = max u 0
                a <- readArray (horiz model) i
                b <- readArray (vert model) i
                let c = (a + b) / 2
                if wall == 0.0 then
                    color $ Color3 0.0 0.0 (0.0 :: Float)
                else
                    if drawingMode == Pressure then
                        color $ uncurry3 Color3 (getSciColour pressure minPressure (maxPressure + 0.01))
                    else if drawingMode == Velocity then
                        color $ Color3 (a / 3 + 0.5) (b / 3 + 0.5) (c / 3 + 0.5)
                    else
                        color $ Color3 smokeNorm smokeNorm smokeNorm
                let screenX = (fromIntegral x / fromIntegral xCells * 2 - 1) :: GLfloat
                let screenY = (fromIntegral y / fromIntegral yCells * 2 - 1) :: GLfloat
                let dx = fromIntegral cellWidth / fromIntegral width * 2.0
                let dy = fromIntegral cellHeight / fromIntegral height * 2.0
                vertex $ Vertex2 screenX screenY
                vertex $ Vertex2 (screenX + dx) screenY
                vertex $ Vertex2 (screenX + dx) (screenY + dy)
                vertex $ Vertex2 screenX (screenY + dy)


getSciColour :: Float -> Float -> Float -> (Float, Float, Float)
getSciColour val minVal maxVal
        | num == 0 = (0.0, s, 1.0)
        | num == 1 = (0.0, 1.0, 1.0 - s)
        | num == 2 = (s, 1.0, 0.0)
        | num == 3 = (1.0, 1.0 - s, 0.0)
        | otherwise = (0.0, 0.0, 0.0)
    where 
        d = maxVal - minVal
        normVal = if d == 0.0 then 0.5 else (val - minVal) / d
        num = fromIntegral $ floor (normVal * 4)
        s = (normVal - num / 4) * 4
        

update :: Model -> IO Model
update model = do
        forM_ [0..yCells - 1] $ \y -> do
            set (horiz model) 1 y 2.0

        let pipeWidth = 9
        forM_ [(yCells `div` 2 - pipeWidth)..(yCells `div` 2 + pipeWidth)] $ \y -> do
            set (smoke model) 1 y 0.0
        -- integrate model -- if you want gravity
        forM_ [0..xCells * yCells - 1] $ \i -> do
            writeArray (pressure model) i 0
        divergence model

        extrapolate model
        advection model
        smokeAdvection model
        return model

extrapolate Model {horiz=horiz, vert=vert} = do
    forM_ [0..xCells - 1] $ \x -> do
        u1 <- at horiz x 1
        set horiz x 0 u1
        u2 <- at horiz x (yCells - 2)
        set horiz x (yCells - 1) u2
    forM_ [0..yCells - 1] $ \y -> do
        v1 <- at vert 1 y
        set vert 0 y v1
        v2 <- at vert (xCells - 2) y
        set vert (xCells - 1) y v2

integrate :: Model -> IO ()
integrate Model {wall=wall, vert=vert} = do
    -- add gravity (not actually used)
    forM_ [1..xCells-1] $ \x -> do
        forM_ [1..yCells-2] $ \y -> do
            wall0 <- at wall x y
            wall1 <- at wall x (y - 1)
            when (wall0 /= 0.0 && wall1 /= 0.0) $
                add vert x y (g * dt)

divergence :: Model -> IO ()
divergence Model {wall=wall, horiz=horiz, vert=vert, pressure=pressure} =
    -- force the fluid to be incompressible
    replicateM_ numIters $
    forM_ [1..xCells-2] $ \x -> do
        forM_ [1..yCells-2] $ \y -> do
            x0 <- at wall (x - 1) y
            x1 <- at wall (x + 1) y
            y0 <- at wall x (y - 1)
            y1 <- at wall x (y + 1)
            let s = x0 + x1 + y0 + y1
            if s == 0 then do
                return ()
            else do
                uxy <-  at horiz x y
                vxy <-  at vert  x y
                ux1y <- at horiz (x + 1) y
                vxy1 <- at vert  x (y + 1)
                -- compute how much fluid is flowing in/out of the cell
                let diverge = ux1y - uxy + vxy1 - vxy
                let p = -diverge / s * 1.9
                -- modify velocities to make sure that the divergence is zero
                add pressure x y (p * coefficientPressure)
                add horiz  x     y    (- (p * x0))
                add horiz (x+1)  y    (  (p * x1))
                add vert   x     y    (- (p * y0))
                add vert   x    (y+1) (  (p * y1))

    where
        coefficientPressure = fluidDensity * cellSpacing / dt

-- helpers for treating a (Float, Float) as a 2d vector
vsub :: Num a => (a, a) -> (a, a) -> (a, a)
(x0, y0) `vsub` (x1, y1) = (x0 - x1, y0 - y1)

vmul :: Num a => (a, a) -> a -> (a, a)
(x0, y0) `vmul` n = (x0 * n, y0 * n)

clamp :: Ord a => a -> a -> a -> a
clamp a lower upper | a > upper = upper
                    | a < lower = lower
                    | otherwise = a

advection :: Model -> IO ()
advection Model {wall=wall, horiz=horiz, vert=vert, newHoriz=newHoriz, newVert=newVert} = do
    forM_ [0..xCells * yCells - 1] $ \i -> do
        u <- readArray horiz i
        v <- readArray vert i
        writeArray newHoriz i u
        writeArray newVert i v
    forM_ [1..xCells-1] $ \x -> do
        forM_ [1..yCells-1] $ \y -> do
            sxy  <- at wall x y
            sx1y <- at wall (x - 1) y
            sxy1 <- at wall x (y - 1)
            -- just make sure we're not processing any walls
            if (sxy /= 0.0 && sx1y /= 0.0 && y < yCells - 1) then do
                -- where are we now?
                let currentPos = (fromIntegral x * cellSpacing,
                                fromIntegral y * cellSpacing + (cellSpacing / 2))
                -- where are we going?
                currentVelocity <- velocityAtU x y
                -- where must we have been, assuming velocity stayed the same?
                let previousPos = currentPos `vsub` (currentVelocity `vmul` dt)
                -- what velocity is at the position we must have just been at?
                u <- uncurry (sample horiz 0.0 (cellSpacing / 2)) previousPos
                -- update current velocity to the velocity of where we must have been
                set newHoriz x y u
            else return ()

            if (sxy /= 0.0 && sxy1 /= 0.0 && x < xCells - 1) then do
                -- ditto but update vertical velocity components
                let currentPos = (fromIntegral x * cellSpacing + (cellSpacing / 2),
                                fromIntegral y * cellSpacing)
                currentVelocity <- velocityAtV x y
                let previousPos = currentPos `vsub` (currentVelocity `vmul` dt)

                v <- uncurry (sample vert (cellSpacing / 2) 0.0) previousPos
                set newVert x y v
            else return () 

    forM_ [0..xCells * yCells - 1] $ \i -> do
        u <- readArray newHoriz i
        v <- readArray newVert i
        writeArray horiz i u
        writeArray vert i v
    where
        velocityAtU x y = do
            -- average vertical components around a single horizontal component
            u <- at horiz x y
            v01 <- at vert (x+1) (y-1)
            v11 <- at vert x (y-1)
            v00 <- at vert (x+1) y
            v10 <- at vert x y
            -- print (v01, v11, v00, v10)
            let v = (v01 + v11 + v00 + v10) / 4
            return (u, v)
        velocityAtV x y = do
            -- average horizontal components around a single vertical component
            v <- at vert x y
            u0 <- at horiz (x-1) (y+1)
            u1 <- at horiz x (y+1)
            u2 <- at horiz (x-1) y
            u3 <- at horiz x y
            let u = (u0 + u1 + u2 + u3) / 4
            return (u, v)

sample :: IOUArray Int Float -> Float -> Float -> Float -> Float -> IO Float
sample field dx dy ix iy = do
    -- make sure the x and y values arent whack
    let x = clamp ix cellSpacing ((fromIntegral xCells - 1) * cellSpacing)
    let y = clamp iy cellSpacing ((fromIntegral yCells - 1) * cellSpacing)
    
    -- which cell is the given coordinate inside?
    let gridX0 = min (floor ((x - dx) / cellSpacing)) (xCells - 1)
    let gridY0 = min (floor ((y - dy) / cellSpacing)) (yCells - 1)
    let gridX1 = gridX0 + 1
    let gridY1 = gridY0 + 1
    -- Offsets of the position into the current cell
    let offsetX = ((x - dx) - (fromIntegral gridX0) * cellSpacing) / cellSpacing
    let offsetY = ((y - dy) - (fromIntegral gridY0) * cellSpacing) / cellSpacing

    -- Weights of each velocity component
    let w00 = 1 - offsetX
    let w01 = offsetX
    let w10 = 1 - offsetY
    let w11 = offsetY

    -- sample each velocity component
    vx0y0 <- at field gridX0 gridY0
    vx1y0 <- at field gridX1 gridY0
    vx0y1 <- at field gridX0 gridY1
    vx1y1 <- at field gridX1 gridY1
    -- final weighted sum
    return ( 
        w00 * w10 * vx0y0 +
        w01 * w10 * vx1y0 +
        w00 * w11 * vx0y1 +
        w01 * w11 * vx1y1)

smokeAdvection :: Model -> IO ()
smokeAdvection Model {wall=wall, horiz=horiz, vert=vert, smoke=smoke, newSmoke=newSmoke} = do
    -- this is basically the same as the previous advection function,
    -- but instead of working with velocities it works with smoke density
    forM_ [0..xCells * yCells - 1] $ \i -> do
        s <- readArray smoke i
        writeArray newSmoke i s
    forM_ [1..xCells-2] $ \x -> do
        forM_ [1..yCells-2] $ \y -> do
            sxy <- at wall x y
            when (sxy /= 0.0 )$ do
                u0 <- at horiz x y
                u1 <- at horiz (x+1) y
                v0 <- at vert x y
                v1 <- at vert x (y+1)
                let currentVelocity = ((u0 + u1) / 2.0, (v0 + v1) / 2.0)
                let currentPos = (fromIntegral x * cellSpacing + cellSpacing / 2.0,
                                  fromIntegral y * cellSpacing + cellSpacing / 2.0)
                let previousPos = currentPos `vsub` (currentVelocity `vmul` dt)
                s <- uncurry (sample smoke (cellSpacing / 2) (cellSpacing / 2)) previousPos
                set newSmoke x y s

    forM_ [0..xCells * yCells - 1] $ \i -> do
        s <- readArray newSmoke i
        writeArray smoke i s
