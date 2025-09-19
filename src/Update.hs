{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Update (updateMain, updateCanvas) where

import Import
import Util

import qualified Graphics.UI.Gtk as GTK 
import Graphics.Rendering.Cairo as C

import Data.Time.Clock (getCurrentTime,diffUTCTime)

-- our internal main loop
updateMain :: RIO App Bool
updateMain = do
  App {..} <- ask
  Zus {..} <- get

  -- set blue according to state and force window redraw
  liftIO $ 
    doFromTo 0 (_appHeight-1) $ \y ->
      doFromTo 0 (_appWidth-1) $ \x ->
        pokeByteOff _stPixBuf (0+x*_appChan+y*_appRow) _stBlue  -- unchecked indexing
    
  -- arrange for the canvas to be redrawn now that we've changed
  -- the Pixbuf
  liftIO $ GTK.widgetQueueDraw _appCanvas
    
  updateBlueVal
  updateFrameCounter
  delayFrame
  return True

-- update the blue state ready for next time
-- change blue value from 'minBound' to 'maxBound' in steps of 'diff'
updateBlueVal :: RIO App ()
updateBlueVal = do
  Zus {..} <- get
  let diff = 1
    
  if _stDir then 
    if _stBlue<=maxBound-diff then
      stBlue %= (+diff)
    else do
      stBlue .= maxBound
      stDir  %= not
  else
    if _stBlue>=minBound+diff then
      stBlue %= (subtract diff)
    else do
      stBlue .= minBound
      stDir  %= not

-- update frame counter
updateFrameCounter :: RIO App ()
updateFrameCounter = do
  Zus {..} <- get
  now <- liftIO $ getCurrentTime
  if diffUTCTime now _stLastSec >= 1.000 then do
    logInfo $ display _stFrames
    stFrames .= 0
    stLastSec .= now
  else do
    stFrames %= (+1)

-- delay to a maximum of 60 fps
-- - prevents more blue steps, when switching scaleFactor
-- - 60 fps is never reached, since this is only called via GTK.idleAdd
-- - with higher prio other stuff stops working (canvas rendering, event handling upto window creation)
delayFrame :: RIO App ()
delayFrame = do
  Zus {..} <- get
  now <- liftIO $ getCurrentTime
  if diffUTCTime now _stLastFrame >= 1/60.0 then do
    stLastFrame .= now
  else do
    liftIO $ threadDelay 500
    delayFrame

-- render the states pixbuf via cairo
updateCanvas :: RIO App (Render ())
updateCanvas = do
  App {..} <- ask
  Zus {..} <- get
  
  return $ do
    C.scale (fromIntegral _stScaleFactor) (fromIntegral _stScaleFactor)
    s <- liftIO $ C.createImageSurfaceForData _stPixBuf C.FormatRGB24 _appWidth _appHeight _appRow
    C.setSourceSurface s 0 0
    C.paint

