{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
module Types
  ( module Types -- export everything or type out millions of lensen :/
  ) where

import RIO (Int,Bool,SomeRef,HasLogFunc(..),LogFunc,HasStateRef(..),lens)
import RIO.Process (ProcessContext, HasProcessContext(..))

import Control.Lens.TH (makeClassy)

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
  , _appState          :: !(SomeRef Zus)
  , _appWidth          :: !Int
  , _appHeight         :: !Int
  , _appChan           :: !Int
  , _appRow            :: !Int
  , _appWindow         :: !Window
  , _appCanvas         :: !DrawingArea
  -- Add other app-specific configuration information here
  }

-- Zustand = State
-- but the lens 'state' would clash with function 'state' from RIO.State
data Zus = Zus
  { _stScaleFactor :: !Int
  , _stPixBuf      :: !(Ptr CUChar)
  , _stBlue        :: !CUChar
  , _stDir         :: !Bool
  , _stFrames      :: !Int
  , _stLastSec     :: !UTCTime
  , _stLastFrame   :: !UTCTime
  }
  

instance HasLogFunc App where
  logFuncL = lens _appLogFunc (\x y -> x { _appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens _appProcessContext (\x y -> x { _appProcessContext = y })
instance HasStateRef Zus App where
  stateRefL = lens _appState (\x y -> x { _appState = y })
  
makeClassy ''App
makeClassy ''Zus
