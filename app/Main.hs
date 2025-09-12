{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
 
import Import
import Run
import Events

import RIO.Process
import RIO.State
import Options.Applicative.Simple
import qualified Paths_yagbe
import Control.Lens hiding (argument, (^.))

import Prelude (print)
import qualified Graphics.UI.Gtk as GTK 
import Graphics.UI.Gtk ( AttrOp(..) )
import Graphics.Rendering.Cairo hiding (x,y)
import Graphics.Rendering.Cairo.Types (PixelData)

--import Prelude
--import Control.Applicative
import Control.Monad (void)
--import Data.IORef
import Data.Time.Clock (getCurrentTime,diffUTCTime)

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
      <*> option auto 
            ( long "scale"
            <> short 's'
            <> showDefault
            <> value 3
            <> metavar "INT"
            <> help "Scale Factor (integer)"
            )
    )
    empty
  
  -- default gb display size
  let w = 160
      h = 144
      -- chan is 4 byte even for 24bit (1 byte unused)
      chan = 4
      row = w * chan

  -- initialize the Pixbuf
  pbData <- liftIO $ mallocArray (w * h * chan)

  doFromTo 0 (h-1) $ \y ->
    doFromTo 0 (w-1) $ \x -> do
      --pokeByteOff pbData (1+x*chan+y*row) (fromIntegral 255 :: CUChar)
      pokeByteOff pbData (2+x*chan+y*row) (fromIntegral x :: CUChar)
      pokeByteOff pbData (1+x*chan+y*row) (fromIntegral y :: CUChar)
      pokeByteOff pbData (0+x*chan+y*row) (0 :: CUChar)

  now <- getCurrentTime
  
  let initState = State {
                    _stScaleFactor = optionsScaleFactor options,
                    _stPixBuf = pbData,
                    _stBlue = 0,
                    _stDir = True,
                    _stFrames = 0,
                    _stLastSec = now,
                    _stLastFrame = now
                  }                   
                  
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  ref <- newSomeRef initState
  
  GTK.initGUI
  window <- GTK.windowNew
  canvas <- GTK.drawingAreaNew
  GTK.set window [GTK.windowDefaultWidth := w*_stScaleFactor initState,
              GTK.windowDefaultHeight := h*_stScaleFactor initState,
              GTK.windowTitle := ("YAGBE"::String),
              GTK.containerChild := canvas]
  
  withLogFunc lo $ \lf ->
    let app = App
          { _appLogFunc = lf
          , _appProcessContext = pc
          , _appOptions = options
          , _appState = ref
          , _appWidth  = w
          , _appHeight = h
          , _appChan  = chan
          , _appRow = row
          , _appWindow = window
          , _appCanvas = canvas
          }
     in runRIO app main' --run

main' :: RIO App ()
main' = do
  app@(App  {..}) <- ask
  st@(State {..}) <- get
  
  registerKeyPress
  
  liftIO $ do
    GTK.idleAdd (runRIO app updateBlue) GTK.priorityDefaultIdle --Low
    
    GTK.on _appCanvas GTK.draw (join $ runRIO app updateCanvas)
    GTK.on _appWindow GTK.objectDestroy destroyEventHandler
    
    --onDestroy window mainQuit 
    --boxPackStart contain canvas PackGrow 0
    GTK.widgetShow _appCanvas
    GTK.widgetShow _appWindow
    GTK.mainGUI
  

updateBlue :: RIO App Bool
updateBlue = do
  app@(App  {..}) <- ask
  st@(State {..}) <- get

  -- set blue according to state and force window redraw
  liftIO $ 
    doFromTo 0 (_appHeight-1) $ \y ->
      doFromTo 0 (_appWidth-1) $ \x ->
        pokeByteOff (st^.stPixBuf) (0+x*app^.appChan+y*app^.appRow) (st^.stBlue)  -- unchecked indexing
    
  -- arrange for the canvas to be redrawn now that we've changed
  -- the Pixbuf
  liftIO $ GTK.widgetQueueDraw _appCanvas
    
  -- update the blue state ready for next time
  -- change blue value from 'minBound' to 'maxBound' in steps of 'diff'
  let diff = 1
    
  if _stDir then 
    if _stBlue<=maxBound-diff then
      stBlue %= (+diff)
    else do
      stBlue %= const maxBound
      stDir  %= not
  else
    if _stBlue>=minBound+diff then
      stBlue %= (subtract diff)
    else do
      stBlue %= const minBound
      stDir  %= not

  -- update frame counter
  now <- liftIO $ getCurrentTime
  if diffUTCTime now _stLastSec >= 1.000 then do
    logInfo $ display _stFrames
    stFrames %= const 0
    stLastSec %= const now
  else do
    stFrames %= (+1)

  -- delay to a maximum of 60 fps
  -- (prevents more blue steps, when switching scaleFactor)
  -- (60 fps is never reached, since this is only called via GTK.idleAdd)
  -- (with higher prio other stuff stops working (canvas rendering, event handling upto window creation))
  let delayFrame = do
        now <- liftIO $ getCurrentTime
        if diffUTCTime now _stLastFrame >= 1/60.0 then do
          stLastFrame %= const now
        else do
          liftIO $ threadDelay 500
          delayFrame
      
  delayFrame
  return True

updateCanvas :: RIO App (Render ())
updateCanvas = do
  app@(App  {..}) <- ask
  st@(State {..}) <- get
  
  return $ do
    scale (fromIntegral _stScaleFactor) (fromIntegral _stScaleFactor)
    s <- liftIO $ createImageSurfaceForData _stPixBuf FormatRGB24 _appWidth _appHeight _appRow
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
