{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where
 
import Import
import Run
import RIO.Process
import Options.Applicative.Simple
import qualified Paths_yagbe

import Prelude (print)
import qualified Graphics.UI.Gtk as GTK 
import Graphics.UI.Gtk ( AttrOp(..) )
import Graphics.Rendering.Cairo hiding (x,y)
import Graphics.Rendering.Cairo.Types (PixelData)

--import Prelude
--import Control.Applicative
import Control.Monad (void)
--import Data.IORef
import Foreign (allocaArray)
import Foreign.Storable (Storable(..))
import Foreign.C (CUChar)
import Data.Time.Clock (getCurrentTime,diffUTCTime)

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
  let kp = GTK.on w GTK.keyPressEvent . GTK.tryEvent

  kp $ do
    "q" <- GTK.eventKeyName
    liftIO $ GTK.mainQuit

  kp $ do
    "a" <- GTK.eventKeyName
    liftIO $ print "a"

  kp $ do
    kv <- GTK.eventKeyName
    liftIO $ print kv

destroyEventHandler :: IO ()
destroyEventHandler = do
  GTK.mainQuit
 
main :: IO ()
main = do
  (options, ()) <- simpleOptions
    $(simpleVersion Paths_yagbe.version)
    "Header for command line arguments"
    "Program description, also for command line arguments"
    (Options
       <$> switch ( long "verbose"
                 <> short 'v'
                 <> help "Verbose output?"
                  )
    )
    empty
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  withLogFunc lo $ \lf ->
    let app = App
          { appLogFunc = lf
          , appProcessContext = pc
          , appOptions = options
          }
     in runRIO app $ liftIO main' --run

main' = do  
  GTK.initGUI
  window <- GTK.windowNew
  canvas <- GTK.drawingAreaNew
  GTK.set window [GTK.windowDefaultWidth := w*scaleFactor,
              GTK.windowDefaultHeight := h*scaleFactor,
              --windowWindowPosition := WinPosCenter,
              GTK.windowTitle := ("YAGBE"::String),
              GTK.containerChild := canvas]


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

    GTK.idleAdd (updateBlue blueRef dirRef lastTimeRef framesRef pbData canvas) GTK.priorityLow
    
    GTK.on canvas GTK.draw (updateCanvas pbData)
    GTK.on window GTK.objectDestroy destroyEventHandler
    keyPress window
    --onDestroy window mainQuit 
    --boxPackStart contain canvas PackGrow 0
    GTK.widgetShow canvas
    GTK.widgetShow window
    GTK.mainGUI

updateBlue blueRef dirRef lastTimeRef framesRef pbData canvas = do
  blue <- readIORef blueRef
          
  -- print blue
  doFromTo 0 (h-1) $ \y ->
    doFromTo 0 (w-1) $ \x ->
      pokeByteOff pbData (0+x*chan+y*row) blue  -- unchecked indexing
    
  -- arrange for the canvas to be redrawn now that we've changed
  -- the Pixbuf
  GTK.widgetQueueDraw canvas
    
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
