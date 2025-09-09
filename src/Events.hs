{-# LANGUAGE OverloadedStrings #-}
module Events where

import Import

import qualified Graphics.UI.Gtk as GTK 

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
 