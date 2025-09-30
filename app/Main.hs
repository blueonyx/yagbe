{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main (main) where
 
import Import
import Update
import Init
import Events
import Util

import Options.Applicative.Simple
import qualified Paths_yagbe

import qualified Graphics.UI.Gtk as GTK 
import Graphics.UI.Gtk ( AttrOp(..) )
import Data.Time.Clock (getCurrentTime)


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
  pbData <- mallocArray (w * h * chan)

  doFromTo 0 (h-1) $ \y ->
    doFromTo 0 (w-1) $ \x -> do
      --pokeByteOff pbData (1+x*chan+y*row) (fromIntegral 255 :: CUChar)
      pokeByteOff pbData (2+x*chan+y*row) (0::CUChar)--fromIntegral x :: CUChar)
      pokeByteOff pbData (1+x*chan+y*row) (0::CUChar)--fromIntegral y :: CUChar)
      pokeByteOff pbData (0+x*chan+y*row) (0 :: CUChar)

  -- initialize the memory (all theoretical 65KiB)
  memory <- mallocArray (2^16)

  now <- getCurrentTime
  
  let initState = Zus {
                    _stScaleFactor = optionsScaleFactor options,
                    _stPixBuf = pbData,
                    _stBlue = 0,
                    _stDir = True,
                    _stFrames = 0,
                    _stLastSec = now,
                    _stLastFrame = now,
                    _stMemory = memory,
                    _stPaletteIndex = 5,
                    _stRenderBuffer = False
                  }                   
                  
  lo <- logOptionsHandle stderr (optionsVerbose options)
  pc <- mkDefaultProcessContext
  ref <- newSomeRef initState
  
  void $ GTK.initGUI
  window <- GTK.windowNew
  canvas <- GTK.drawingAreaNew
  GTK.set window [GTK.windowDefaultWidth := w*_stScaleFactor initState,
              GTK.windowDefaultHeight := h*_stScaleFactor initState,
              GTK.windowTitle := ("YAGBE"::String),
              GTK.containerChild := canvas]
  
  withLogFunc lo $ \lf ->
    let anw = App
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
     in runRIO anw main' --run

main' :: RIO App ()
main' = do
  anw@(App  {..}) <- ask
  
  registerKeyPress
  
  initGBE
  
  liftIO $ do
    void $ GTK.idleAdd (runRIO anw updateMain) GTK.priorityDefaultIdle --Low
    
    void $ GTK.on _appCanvas GTK.draw (join $ runRIO anw updateCanvas)
    void $ GTK.on _appWindow GTK.objectDestroy destroyEventHandler
    
    GTK.widgetShow _appCanvas
    GTK.widgetShow _appWindow
    GTK.mainGUI
  
