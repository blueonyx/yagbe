{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Init (initGBE) where

import Import

-- initialize ALL the Game Boy Emulation state
initGBE :: RIO App ()
initGBE = return ()
