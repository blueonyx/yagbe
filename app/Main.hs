{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Graphics.UI.Gtk hiding (rectangle)
import Graphics.Rendering.Cairo hiding (x,y)
import Graphics.Rendering.Cairo.Types (PixelData)

import Prelude
import Control.Applicative
import Control.Monad (void)
import Data.IORef
import Foreign (allocaArray)
import Foreign.Storable (Storable(..))
import Foreign.C (CUChar)
import Data.Time.Clock (getCurrentTime,diffUTCTime)

import Lib

scaleFactor :: Int
scaleFactor = 3

w = 160
h = 144
-- chan is 4 byte even for (1 byte unused)
chan = 4
row = w * chan
stride = row

{-keyPress :: EventM EKey Bool
{-keyPress = do
  kv <- eventKeyVal
  tryEvent $ do
    Just 'q' <- keyvalToChar kv
    liftIO mainQuit
-}
keyPress = do
  tryEvent $ do
    "q" <- eventKeyName
    liftIO $ print "quit"
    stopEvent
  tryEvent $ do
    "a" <- eventKeyName
    liftIO $ print "a"
    stopEvent
  tryEvent $ do
    kv <- eventKeyName
    liftIO $ print kv
-}

--keyPress :: _
keyPress w = void $ do
  let kp = on w keyPressEvent . tryEvent

  kp $ do
    "q" <- eventKeyName
    liftIO $ mainQuit

  kp $ do
    "a" <- eventKeyName
    liftIO $ print "a"

  kp $ do
    kv <- eventKeyName
    liftIO $ print kv

destroyEventHandler :: IO ()
destroyEventHandler = do
  mainQuit

main :: IO ()
main = do  
  initGUI
  window <- windowNew
  canvas <- drawingAreaNew
  set window [windowDefaultWidth := w*scaleFactor,
              windowDefaultHeight := h*scaleFactor,
              --windowWindowPosition := WinPosCenter,
              windowTitle := ("YAGBE"::String),
              containerChild := canvas]


  -- create the Pixbuf
  allocaArray (w * h * chan) $ \ pbData -> do

    -- draw into the Pixbuf
    doFromTo 0 (h-1) $ \y ->
      doFromTo 0 (w-1) $ \x -> do
        --pokeByteOff pbData (1+x*chan+y*row) (fromIntegral 255 :: CUChar)
        pokeByteOff pbData (2+x*chan+y*row) (fromIntegral x :: CUChar)
        pokeByteOff pbData (1+x*chan+y*row) (fromIntegral y :: CUChar)
        pokeByteOff pbData (0+x*chan+y*row) (0 :: CUChar)

    -- a function to update the Pixbuf
    blueRef <- newIORef (0 :: CUChar)
    dirRef <- newIORef True
    framesRef <- newIORef 0
    lastTimeRef <- newIORef =<< getCurrentTime

    idleAdd (updateBlue blueRef dirRef lastTimeRef framesRef pbData canvas) priorityLow
    
    canvas `on` draw $ updateCanvas pbData
    window `on` objectDestroy $ destroyEventHandler
    keyPress window
    --onDestroy window mainQuit 
    --boxPackStart contain canvas PackGrow 0
    widgetShow canvas
    widgetShow window
    mainGUI

updateBlue blueRef dirRef lastTimeRef framesRef pbData canvas = do
  blue <- readIORef blueRef
          
  -- print blue
  doFromTo 0 (h-1) $ \y ->
    doFromTo 0 (w-1) $ \x ->
      pokeByteOff pbData (0+x*chan+y*row) blue  -- unchecked indexing
    
  -- arrange for the canvas to be redrawn now that we've changed
  -- the Pixbuf
  widgetQueueDraw canvas
    
  -- update the blue state ready for next time
  dir <- readIORef dirRef
  let diff = 1
  let blue' = if dir then blue+diff else blue-diff
  if dir then
    if blue<=maxBound-diff then writeIORef blueRef blue' else
      writeIORef blueRef maxBound >> modifyIORef dirRef not
  else
    if blue>=minBound+diff then writeIORef blueRef blue' else
      writeIORef blueRef minBound >> modifyIORef dirRef not
      
  lastTime <- readIORef lastTimeRef
  now <- getCurrentTime
  if diffUTCTime now lastTime >= 1.000 then do
    print =<< readIORef framesRef
    writeIORef framesRef 0
    writeIORef lastTimeRef now
  else do
    modifyIORef framesRef (1+)
  return True

updateCanvas :: PixelData -> Render ()
updateCanvas pb = do
  scale (fromIntegral scaleFactor) (fromIntegral scaleFactor)
  s <- liftIO $ createImageSurfaceForData pb FormatRGB24 w h stride
  setSourceSurface s 0 0
  paint

-- GHC is much better at opimising loops like this:
--
-- > doFromTo 0 255 $ \y ->
-- >   doFromTo 0 255 $ \x -> do ...
--
-- Than it is at optimising loops like this:
--
-- > sequence_ [ do ...
-- >           | x <- [0..255]
-- >           , y <- [0..255] ]
--
-- The first kind of loop runs significantly faster (with GHC 6.2 and 6.4)

{-# INLINE doFromTo #-}
-- do the action for [from..to], ie it's inclusive.
doFromTo :: Int -> Int -> (Int -> IO ()) -> IO ()
doFromTo from to action =
  let loop n | n > to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from
