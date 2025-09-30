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
  { _stScaleFactor  :: !Int
  , _stPixBuf       :: !(Ptr CUChar)
  , _stBlue         :: !CUChar
  , _stDir          :: !Bool
  , _stFrames       :: !Int
  , _stLastSec      :: !UTCTime
  , _stLastFrame    :: !UTCTime
  , _stMemory       :: !(Ptr CUChar)
  , _stPaletteIndex :: !Int
  , _stRenderBuffer :: !Bool
  }

instance HasLogFunc App where
  logFuncL = lens _appLogFunc (\x y -> x { _appLogFunc = y })
instance HasProcessContext App where
  processContextL = lens _appProcessContext (\x y -> x { _appProcessContext = y })
instance HasStateRef Zus App where
  stateRefL = lens _appState (\x y -> x { _appState = y })

-- constant game boy addresses
tileBufferAddress :: Int
tileBufferAddress = 0x9000

-- https://gbdev.io/pandocs/Tile_Maps.html
-- The Game Boy contains two 32×32 tile maps in VRAM at the memory areas $9800-$9BFF and $9C00-$9FFF.
-- Any of these maps can be used to display the Background or the Window.
-- NOTE NOT IMPLEMENTED
-- Tile Indexes
-- Each tile map contains the 1-byte indexes of the tiles to be displayed.
-- Tiles are obtained from the Tile Data Table using either of the two addressing modes (described in VRAM Tile Data), which can be selected via the LCDC register.
-- Since one tile has 8×8 pixels, each map holds a 256×256 pixels picture. Only 160×144 (20x18 tiles) of those pixels are displayed on the LCD at any given time.
tileMapAddress :: Int
tileMapAddress = 0x9800

-- BG palette data 
--              7  6 5  4 3  2 1  0
-- Color for...	ID 3 ID 2 ID 1 ID 0
-- 0 White, 1 Light gray, 2 Dark gray, 3 Black
bgPaletteAddress :: Int
bgPaletteAddress = 0xFF47

-- palettes for objects
-- lowest bits ignored, since used as transparent
obp0PaletteAddress,obp1PaletteAddress :: Int
obp0PaletteAddress = 0xFF48
obp1PaletteAddress = 0xFF49

-- gbc only has RGB555 but for now lets use 24bit here
bgPalettes :: [[[CUChar]]]
bgPalettes = [[ [255,255,255]
            , [170,170,170]
            , [ 85, 85, 80]
            , [  0,  0,  0]
            ],

            [ [0,0,255]
            , [0,0,170]
            , [0,0, 85]
            , [0,0,  0]
            ],
            -- niceish blues?
            [ [208,224,248]
            , [112,136,192]
            , [ 86, 52,104]
            , [ 32,  8, 24]
            ],
            -- niceish blues?
            [ [224,208,248]
            , [136,112,192]
            , [ 52, 86,104]
            , [  8, 32, 24]
            ],

            -- simple greens
            [ [  0,255,  0]
            , [  0,170,  0]
            , [  0, 85,  0]
            , [  0, 30,  0]
            ],
            -- nice greens
            [ [224,248,208]
            , [136,192,112]
            , [ 52,104, 86]
            , [  8, 24, 32]
            ],
            
            [ [0xff,0,0]
            , [170,0,0]
            , [ 85,0,0]
            , [  0,0,0]
            ],


            [ [255,0,255]
            , [170,0,170]
            , [ 85,0, 85]
            , [  0,0,  0]
            ],
            
            [ [0,255,255]
            , [0,170,170]
            , [0, 85, 85]
            , [0,  0,  0]
            ],

            [ [255,255,0]
            , [170,170,0]
            , [ 85, 85,0]
            , [  0,  0,0]
            ]
            ]

bgPaletteCount :: Int
bgPaletteCount = 10

makeClassy ''App
makeClassy ''Zus
