module Lib where

-- import System.Random.Mersenne
import System.Random
import Graphics.UI.GLUT
import Data.IORef
import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.IO
import Data.Array.MArray
import Data.UnixTime
import Debug.Trace
import qualified Data.Char as DC

{-
- - Control
-      N     - Move the block to left.
-      M     - Move the block to right.
-      Z     - Rotate.
-      X     - Rotate.
-      Space - Soft Drop.
-}

width   = 7
height  = 15
ceil = 10
boxSize = 0.15  :: Double
boxDiff = boxSize / 10 :: Double
timeDiff = UnixDiffTime 1 0 :: UnixDiffTime
frameDiff = UnixDiffTime 0 100 :: UnixDiffTime

mapBlock :: (Int -> Int -> Bool -> IO ()) -> (Int, Int) -> (Int, Int) -> IOUArray (Int, Int) Bool -> IO ()
mapBlock func (iBgn, iEnd) (jBgn, jEnd) x_ =
    forM_ [iBgn..iEnd] $ \i ->
      forM_ [jBgn..jEnd] $ \j -> readArray x_ (i, j) >>= func i j

-- Copy ONLY 'True' blocks in x_ into y
copyTrueBlock :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> IO ()
copyTrueBlock x_ y = mapBlock (\i j x -> when x (writeArray y (i, j) True)) (0, height) (0, width) x_

-- Called when the window is resized.
resize :: Size -> IO()
resize s@(Size w h) = do
    viewport $= (Position 0 0, s)
    loadIdentity
    ortho (-w') w' (-h') h' (-1.0) 1.0
    where w' = realToFrac w / 200.0
          h' = realToFrac h / 200.0

-- Draw a square at (x, y)
-- (x, y) is a coordinate of upper-left corner.
drawSq :: Double -> Double -> DisplayCallback
drawSq x y = renderPrimitive LineLoop $ mapM_ vertex2d [
                Vertex2 x y
              , Vertex2 (x+sz) y
              , Vertex2 (x+sz) (y+sz)
              , Vertex2 x (y+sz)
              ]
            where sz = boxSize :: Double
                  vertex2d = vertex :: Vertex2 GLdouble -> IO()

-- Convert Int^2 coordinate into GLdouble^2
toGLdoubleCor:: (Int, Int) -> (GLdouble, GLdouble)
toGLdoubleCor (_x, _y) = (x', y')
  where x' = -0.9 + boxDiff*(x+1) + boxSize*x
        y' = -0.9 + boxDiff*(y+1) + boxSize*y
        x  = realToFrac _x :: GLdouble
        y  = realToFrac _y :: GLdouble

-- Draw a square at (Int, Int).
drawBox :: IOUArray (Int, Int) Bool -> DisplayCallback
drawBox = mapBlock func (0, height) (0, width)
    where func i j x = when x $ let (a,b) = toGLdoubleCor (j, i) in drawSq a b


-- Fall blocks upper than y=h+1.(where y is the vertical line)
fillErasedLine :: IOUArray (Int, Int) Bool -> Int -> IO ()
fillErasedLine x h = mapBlock func (h+1, height) (0, width) x
    where
      func :: Int -> Int -> Bool -> IO ()
      func i j y = when y $ do
        writeArray x (i, j) False
        writeArray x (i-1, j) True

-- Erase filled lines
eraseLine :: IOUArray (Int, Int) Bool -> IO ()
eraseLine x_ =
    forM_ [0..height] $ \i -> do
      x <- getElems x_
      -- get i-th line, and apply 'and'. (get True if the line is filled)
      let y = and $ drop ((width+1)*i) $ take ((width+1)*(i+1)) x
      when y $ do
        -- print $ show i++show x
        forM_[0..width] $ \j -> writeArray x_ (i, j) False
        fillErasedLine x_ i

-- Spawn Block!
spawnBox :: IOUArray (Int, Int) Bool -> IO ()
spawnBox x = do
    r <- randomRIO (0,0)
    case (r :: Int) of
      0 -> do
        t (spawnY, 2)
        t (spawnY, 3)
        t (spawnY, 4)
        t (spawnY, 5)
      1 -> do
        t (spawnY-1, 2)
        t (spawnY-1, 3)
        t (spawnY, 3)
        t (spawnY, 4)
      _ -> writeArray x (5, 3) True
      where
            spawnY = ceil
            t :: (Int, Int) -> IO ()
            t y = writeArray x y True

fallBox :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> IO Bool
fallBox _env _x = do
    isonground <- isOnGround _x
    if isonground
      then return True
      else do
        mapBlock mapBlockFunc (0, height) (0, width) _x
        return True
    return isonground
    where 
          mapBlockFunc :: Int -> Int -> Bool -> IO ()
          mapBlockFunc i j x =
            when (x && i /= 0) $ -- i == 0 means the block is on the ground
              do
                writeArray _x (i,j)   False
                writeArray _x (i-1,j) True

          isOnGround :: IOUArray (Int, Int) Bool -> IO Bool -- 'Is on the ground' or 'Is on a block'
          isOnGround xxx = do
            tmpp <- newIORef False
            __x <- getElems _x
            if (not . or) (take (width+1) __x) -- IF NOT the block is on the ground
              then do
                mapBlock
                  (\i j a -> do
                    b <- readArray _env (i-1, j)
                    when (a && b) (writeIORef tmpp True))
                  (1, height)
                  (0, width)
                  xxx
                readIORef tmpp
              else
                return True -- IF the block is on the ground -}


display :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> IORef UnixTime -> DisplayCallback
display dt bl ot = do
    clear [ColorBuffer]
    oldTime <- get ot

    loadIdentity

    -- Draw a box
    color (Color3 1 1 1 :: Color3 GLdouble)
    renderPrimitive Lines $ mapM_ vertex2d [
        Vertex2 (-0.9) (-0.9)
      , Vertex2 (-0.9) 0.8
      ]
    renderPrimitive Lines $ mapM_ vertex2d [
        Vertex2 (-0.9) (-0.9)
      , Vertex2 (0.42 + boxDiff)  (-0.9)
      ]
    renderPrimitive Lines $ mapM_ vertex2d [
        Vertex2 (0.42 + boxDiff) (-0.9)
      , Vertex2 (0.42 + boxDiff)  0.8
      ]

    -- Update time
    unixTime <- getUnixTime
    let diffTime = diffUnixTime unixTime oldTime
    when (timeDiff <= diffTime) $ do
        b <- fallBox dt bl
        writeIORef ot unixTime
        when b $ do
          copyTrueBlock bl dt       -- Copy bl to dt
          clearBl bl
          forM_ [0..ceil] $ (const . eraseLine) dt
          spawnBox bl
    color (Color3 0 1 1 :: Color3 GLdouble)
    drawBox bl
    color (Color3 1 1 0 :: Color3 GLdouble)
    drawBox dt
    flush
    where vertex2d = vertex :: Vertex2 GLdouble -> IO()
          dif = 0.02
          clearBl :: IOUArray (Int, Int) Bool -> IO ()
          clearBl q =
            forM_ [0..height] $ \i ->
              forM_ [0..width] $ \j -> do
                p <- readArray q (i, j)
                when p $ writeArray q (i, j) False

--
isOutOfLeague :: Int -> Int -> Bool
isOutOfLeague x y =
    (x > width)  ||
    (x < 0)      ||
    (y > height) ||
    (y < 0)

rotateRight :: IOUArray (Int, Int) Bool -> IO ()
rotateRight x_ = do
    tmpx_ <- newArray ((0,0), (height,width)) False :: IO (IOUArray (Int, Int) Bool)
    ct <- getCenter x_
    let (cy, cx) = ct -- Center of rotation
    mapBlock (\i j x ->
        when x $
          let (dy, dx) = (cy - i, cx - j)
            in unless (isOutOfLeague (cx + dy) (cy - dx)) $ writeArray tmpx_ (cy-dx, cx+dy) True) (0, height) (0, width) x_
    mapBlock (\i2 j2 x2 ->(writeArray x_ (i2, j2) x2)) (0, height) (0, width) tmpx_

canRotateRight :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> IO Bool
canRotateRight env_ x_ = do
    result <- newIORef True
    ct <- getCenter x_
    let (cy, cx) = ct
    mapBlock (\i j x -> when x (
      let (dy, dx) = (cy - i, cx - j) in
      if isOutOfLeague (cx + dy) (cy - dx)
        then writeIORef result False
        else do
          tmp1 <- readArray env_ (cy-dx, cx+dy)
          when tmp1 $ writeIORef result False
      )) (0,height) (0,width) x_
    readIORef result


rotateLeft :: IOUArray (Int, Int) Bool -> IO ()
rotateLeft x_ = do
    tmpx_ <- newArray ((0,0), (height,width)) False :: IO (IOUArray (Int, Int) Bool)
    ct <- getCenter x_
    let (cy, cx) = ct -- Center of rotation
    mapBlock (\i j x ->
        when x $
          let (dy, dx) = (cy - i, cx - j)
            in unless (isOutOfLeague (cx-dy) (cy+dx)) $ writeArray tmpx_ (cy+dx, cx-dy) True) (0, height) (0, width) x_
    mapBlock (\i2 j2 x2 ->(writeArray x_ (i2, j2) x2)) (0, height) (0, width) tmpx_

canRotateLeft :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> IO Bool
canRotateLeft env_ x_ = do
    result <- newIORef True
    ct <- getCenter x_
    let (cy, cx) = ct
    mapBlock (\i j x -> when x (
      let (dy, dx) = (cy - i, cx - j) in
      if isOutOfLeague (cx-dy) (cy+dx)
        then writeIORef result False
        else do
          tmp1 <- readArray env_ (cy+dx, cx-dy)
          print tmp1
          when tmp1 $ writeIORef result False
      )) (0,height) (0,width) x_
    readIORef result



-- Arg 'x_' : Falling Block
-- Return : Center of the block (y, x). take an average of the position of block
getCenter :: IOUArray (Int, Int) Bool -> IO (Int, Int)
getCenter x_ = do
    tmpY <- newIORef 0
    tmpX <- newIORef 0
    tmpY2 <- newIORef 0
    tmpX2 <- newIORef 0
    mapBlock (\i j x -> when x (do
              modifyIORef tmpY (+i)
              modifyIORef tmpX (+j)
              modifyIORef tmpY2 (+1)
              modifyIORef tmpX2 (+1)
            )) (0,height) (0,width) x_
    a <- readIORef tmpY
    b <- readIORef tmpX
    c <- readIORef tmpY2
    d <- readIORef tmpX2
    return (a `div` c, b `div` d) -- take average


-- Return True if the block cannot move
cannotItMove :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> Int -> Int -> IO Bool
cannotItMove env_ ub_ directionX directionY = do
    tmpp <- newIORef False
    mapBlock
      (\i j a -> do
                b <- readArray env_ (i+directionY, j+directionX)
                when (a && b) (writeIORef tmpp True) )
      (0, height - abs directionY)
      (0, width - abs directionX)
      ub_
    readIORef tmpp



keyboard :: IORef UnixTime -> IORef Char -> IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> KeyboardCallback
keyboard tw x_ env_ userblock ch state = do
    now <- getUnixTime
    ttt <- readIORef tw
    when (diffUnixTime now  ttt >= frameDiff) $ do
      x <- readIORef x_
      keyboard_ x_ env_ userblock ch state
    writeIORef x_ ch

moveItLeft :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> IO ()
moveItLeft env_ userblock_ = mapBlock func (0, height) (0, width) userblock_
    where
      func :: Int -> Int -> Bool -> IO ()
      func i j v =
        when v $ do
          writeArray userblock_ (i, j)   False
          writeArray userblock_ (i, j-1) True

moveItRight :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> IO ()
moveItRight env_ userblock_ = mapBlock func (0, height) (0, width) userblock_
    where
      func :: Int -> Int -> Bool -> IO ()
      func _i _j _ = do
        let (i,j) = (height-_i, width-_j)
        v <- readArray userblock_ (i, j)
        when v $
          do
            print $ show i++show j
            writeArray userblock_ (i, j) False
            writeArray userblock_ (i, j+1) True


hardDrop :: IOUArray (Int, Int) Bool -> IO ()
hardDrop userblock_ = mapBlock (\i j x -> when x (do
                                                    writeArray userblock_ (0, j) True
                                                    writeArray userblock_ (i, j) False))
                                (0, height) (0, width) userblock_


keyboard_ :: IORef Char -> IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> KeyboardCallback
keyboard_ oldkeyCh_ env_ userblock_ ch state
  | ch == 'q' = leaveMainLoop -- quit
  | ch == 'm' = do -- move right
    j <- forM [0..height] $ \i -> do -- If the block is at the end of the stage
      v <- readArray userblock_ (i, width)
      if v then return True else return False
    j2 <- cannotItMove env_ userblock_ 1 0
    unless (True `elem` j || j2) $ moveItRight env_ userblock_
        
  | ch == 'n' = do -- Move left
    j <- forM [0..height] $ \i -> do
      v <- readArray userblock_ (i, 0)
      if v then return True else return False
    j2 <- cannotItMove2 env_ userblock_ (-1)
    unless (True `elem` j || j2) $ moveItLeft env_ userblock_
  | ch == 'x' = do -- Rotate2
    canrotate <- canRotateLeft env_ userblock_
    print $ "canrotateLeft="++show canrotate
    when canrotate $ rotateLeft userblock_
  | ch == 'z' = do -- Rotate1
    canrotate <- canRotateRight env_ userblock_
    print $ "canrotate="++show canrotate
    when canrotate $ rotateRight userblock_
  | ch == ' ' = do -- Soft drop.
    fallBox env_ userblock_
    return ()
  | ch == 'j' = hardDrop userblock_
  | otherwise =
    print $ DC.ord ch
  where
    cannotItMove2 :: IOUArray (Int, Int) Bool -> IOUArray (Int, Int) Bool -> Int -> IO Bool
    cannotItMove2 env_ ub_ directionX = do
      tmpp <- newIORef False
      mapBlock (\i j a -> do
                  b <- readArray env_ (i, j+directionX)
                  when (a && b) $ writeIORef tmpp True)
               (0, height)
               (1, width)
               ub_
      readIORef tmpp


idle :: IdleCallback
idle = postRedisplay Nothing

someFunc :: IO ()
someFunc = do
    getArgsAndInitialize
    initialDisplayMode $= [RGBAMode]
    createWindow "Sirtet"
    clearColor $= Color4 0 0 0 0
    uxTime <- getUnixTime
    nxTime <- getUnixTime
    dt <- newArray ((0,0), (height,width)) False :: IO (IOUArray (Int, Int) Bool)
    bl <- newArray ((0,0), (height,width)) False :: IO (IOUArray (Int, Int) Bool)
      -- Index (y,x)
      -- value = 'True' if the block exists there
    tm <- newIORef uxTime
    tw <- newIORef nxTime
    oldCh <- newIORef '\n'

    spawnBox bl

    displayCallback  $= display dt bl tm
    reshapeCallback  $= Just resize
    keyboardCallback $= Just (keyboard tw oldCh dt bl)
    idleCallback     $= Just idle
    mainLoop



