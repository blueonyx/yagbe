{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Events where

import Import

import Control.Lens
import RIO.State
import qualified Graphics.UI.Gtk as GTK 

-- quit on window close
destroyEventHandler :: IO ()
destroyEventHandler = do
  GTK.mainQuit

registerKeyPress :: RIO App ()
registerKeyPress = do
  app@(App  {..}) <- ask
  st@(State {..}) <- get
  
  let kp = liftIO . void . GTK.on _appWindow GTK.keyPressEvent . GTK.tryEvent
  
  -- quit
  kp $ do
    "q" <- GTK.eventKeyName
    liftIO $ GTK.mainQuit

  -- increase scaleFactor
  kp $ do
    "f" <- GTK.eventKeyName
    liftIO $ runRIO app $ do
      sf <- stScaleFactor <%= (+1)
      liftIO $ GTK.windowResize _appWindow (_appWidth*sf) (_appHeight*sf)
  
  -- decrease scaleFactor
  kp $ do
    "g" <- GTK.eventKeyName
    liftIO $ runRIO app $ do
      sf <- stScaleFactor <%= (\x -> if x > 1 then x-1 else x)
      liftIO $ GTK.windowResize _appWindow (_appWidth*sf) (_appHeight*sf)

  -- log keyPress
  kp $ do
    kv <- GTK.eventKeyName
    liftIO $ runRIO app $ logInfo $ displayShow kv
