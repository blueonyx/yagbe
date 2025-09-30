{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Update (updateMain, updateCanvas) where

import Import
import Util

import RIO.List.Partial ((!!))
import Data.Bits (testBit)
import qualified Graphics.UI.Gtk as GTK 
import Graphics.Rendering.Cairo as C

import Data.Time.Clock (getCurrentTime,diffUTCTime)

-- our internal main loop
updateMain :: RIO App Bool
updateMain = do
  App {..} <- ask
  Zus {..} <- get

  when   _stRenderBuffer $ renderTileBuffer
  unless _stRenderBuffer $ renderTileMap

  --setBlueVal
  
  -- arrange for the canvas to be redrawn now that we've changed
  -- the Pixbuf
  liftIO $ GTK.widgetQueueDraw _appCanvas
   
  --updateBlueVal   
  updateFrameCounter
  delayFrame
  return True

-- write tiles according to tilemap and palette from _stMemory into _stMemory
renderTileMap :: RIO App ()
renderTileMap = do
  App {..} <- ask
  Zus {..} <- get

  -- copy 20x18 tiles from tileMap indices via tileBuffer into Pixbuf using the current palette
  --pal <- liftIO $ peekByteOff _stMemory bgPaletteAddress :: RIO App CUChar
  
  doFromTo 0 17 $ \y -> do
    doFromTo 0 19 $ \x -> do
      -- the tileMap is actually 32x32 tiles, although only 20x18 can be displayed
      (tileIndex :: CUChar) <- liftIO $ peekByteOff _stMemory $ tileMapAddress+y*32+x

      renderTile (tileIndexToAddr $ fromIntegral tileIndex) x y

-- render the tile at addr to position (x,y) in Pixbuf
renderTile :: Int -> Int -> Int -> RIO App ()
renderTile addr x y = do
  App {..} <- ask
  Zus {..} <- get

  liftIO $
    doFromTo 0 7 $ \row -> do
      low  <- peekByteOff _stMemory $ addr+2*row+0 :: IO CUChar
      high <- peekByteOff _stMemory $ addr+2*row+1 :: IO CUChar

      doFromTo 0 7 $ \col -> do
        let paletteIndex = paletteIndexAt low high col
            [r,g,b] = (bgPalettes !! _stPaletteIndex) !! paletteIndex

        -- set the right colors in the pixbuf
        pokeByteOff _stPixBuf (0+(x*8+col)*_appChan+(y*8+row)*_appRow) b
        pokeByteOff _stPixBuf (1+(x*8+col)*_appChan+(y*8+row)*_appRow) g
        pokeByteOff _stPixBuf (2+(x*8+col)*_appChan+(y*8+row)*_appRow) r

-- combine the 2 bit paletteIndex from the 2 bytes of a row of tile Data
-- https://gbdev.io/pandocs/Tile_Data.html
-- note to switch endianess of col
paletteIndexAt :: CUChar -> CUChar -> Int -> Int
paletteIndexAt low high col =
  2*(fromEnum $ high `testBit` col') + (fromEnum $ low `testBit` col')
    where col' = 7-col

-- tiles are 16 bytes and start at tileBufferAddress
tileIndexToAddr :: Int -> Int
tileIndexToAddr i = tileBufferAddress + 16*fromIntegral i

-- write tiles according to tilebuffer and palette from _stMemory into _stMemory
-- so just rendering the tiles 0..20*18-1
renderTileBuffer :: RIO App ()
renderTileBuffer = do
  doFromTo 0 17 $ \y -> do
    doFromTo 0 19 $ \x -> do
      let tileAddr = tileIndexToAddr $ y*20+x
      renderTile tileAddr x y

-- fill change Pixbuf with new blue value
setBlueVal :: RIO App ()
setBlueVal = do
  App {..} <- ask
  Zus {..} <- get
  -- set blue according to state and force window redraw
  liftIO $ 
    doFromTo 0 (_appHeight-1) $ \y ->
      doFromTo 0 (_appWidth-1) $ \x ->
        pokeByteOff _stPixBuf (0+x*_appChan+y*_appRow) _stBlue  -- unchecked indexing
  

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

