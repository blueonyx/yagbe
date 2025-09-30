{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Events 
  ( destroyEventHandler
  , registerKeyPress
  ) where

import Import

import qualified Graphics.UI.Gtk as GTK 

-- quit on window close
destroyEventHandler :: IO ()
destroyEventHandler = do
  GTK.mainQuit

registerKeyPress :: RIO App ()
registerKeyPress = do
  anw@(App  {..}) <- ask
  
  let kp = liftIO . void . GTK.on _appWindow GTK.keyPressEvent . GTK.tryEvent
  
  -- quit
  kp $ do
    "q" <- GTK.eventKeyName
    liftIO $ GTK.mainQuit

  -- increase scaleFactor
  kp $ do
    "f" <- GTK.eventKeyName
    liftIO $ runRIO anw $ do
      sf <- stScaleFactor <%= (+1)
      liftIO $ GTK.windowResize _appWindow (_appWidth*sf) (_appHeight*sf)
  
  -- decrease scaleFactor
  kp $ do
    "g" <- GTK.eventKeyName
    liftIO $ runRIO anw $ do
      sf <- stScaleFactor <%= (\x -> if x > 1 then x-1 else x)
      liftIO $ GTK.windowResize _appWindow (_appWidth*sf) (_appHeight*sf)
      
  -- next bgPalette
  kp $ do
    "t" <- GTK.eventKeyName
    liftIO $ runRIO anw $ 
      stPaletteIndex %= (\x -> if x+1 == bgPaletteCount then 0 else x+1)

  -- prev bgPalette
  kp $ do
    "r" <- GTK.eventKeyName
    liftIO $ runRIO anw $
      stPaletteIndex %= (\x -> if x == 0 then bgPaletteCount-1 else x-1)

  -- log keyPress
  kp $ do
    kv <- GTK.eventKeyName
    liftIO $ runRIO anw $ logInfo $ displayShow kv
