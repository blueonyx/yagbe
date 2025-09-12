{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( module Types
  ) where

import RIO
import RIO.Process

import Control.Lens.TH

import Data.Time.Clock (UTCTime)
import Foreign.Ptr (Ptr)
import Foreign.C (CUChar)
import Graphics.UI.Gtk (DrawingArea, Window)

-- | Command line arguments
data Options = Options
  { optionsVerbose     :: !Bool
  , optionsScaleFactor :: !Int
  }

data App = App
  { _appLogFunc        :: !LogFunc
  , _appProcessContext :: !ProcessContext
  , _appOptions        :: !Options
  , _appState          :: !(SomeRef State)
  , _appWidth          :: !Int
  , _appHeight         :: !Int
  , _appChan           :: !Int
  , _appRow            :: !Int
  , _appWindow         :: !Window
  , _appCanvas         :: !DrawingArea
  -- Add other app-specific configuration information here
  }
  

  
data State = State
  { _stScaleFactor :: !Int
  , _stPixBuf      :: !(Ptr CUChar)
  , _stBlue        :: !CUChar
  , _stDir         :: !Bool
  , _stFrames      :: !Int
  , _stLastTime    :: !UTCTime
  }
  

instance HasLogFunc App where
  logFuncL = lens _appLogFunc (\x y -> x { _appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens _appProcessContext (\x y -> x { _appProcessContext = y })
instance HasStateRef State App where
  stateRefL = lens _appState (\x y -> x { _appState = y })
  
makeClassy ''App  
makeClassy ''State
