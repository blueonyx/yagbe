{-# LANGUAGE NoImplicitPrelude #-}
-- | Silly utility module, used to demonstrate how to write a test
-- case.
module Util
  ( doFromTo
  , paletteIndexAt
  , paletteIndexRowToBytes
  , objTileIndexToAddr
  , bgTileIndexToAddr
  ) where


import Import



-- combine the 2 bit paletteIndex from the 2 bytes of a row of tile Data
-- https://gbdev.io/pandocs/Tile_Data.html#data-format
-- note to switch endianess of col
paletteIndexAt :: CUChar -> CUChar -> Int -> Int
paletteIndexAt low high col =
  2*(fromEnum $ high `testBit` col') + (fromEnum $ low `testBit` col')
    where col' = 7-col
    
-- split a row of palette indices (0,1,2 or 3) into the 2 byte representation
-- https://gbdev.io/pandocs/Tile_Data.html#data-format
-- equal to RGBASM's 'dw `xxxxyyyy' shorthand
--
-- e.g. dw `02333320 => [0x3C,0x7E] / [60,126]
paletteIndexRowToBytes :: Int -> [CUChar]
paletteIndexRowToBytes row = [ low, high]
  where
    pis = toPaletteIndices 8 [] row
    
    (high, low) = indicesToBytes (0,0) pis
    
    indicesToBytes res [] = res
    indicesToBytes (h,l) (i:is) = indicesToBytes (h'',l'') is
      where 
        (h',l') = (h `shiftL` 1, l `shiftL` 1)
        (h'', l'') = case i of
                      0 -> (h',  l'  )
                      1 -> (h',  l'+1)
                      2 -> (h'+1,l'  )
                      3 -> (h'+1,l'+1)
    
    toPaletteIndices 0 res _ = res
    toPaletteIndices n res num = toPaletteIndices (n-1) (p:res) num'
      where (num', p) = num `divMod` 10
            

-- tiles are 16 bytes and start at bgTileBufferAddress
bgTileIndexToAddr :: Int -> Int
bgTileIndexToAddr i = bgTileBufferAddress + 16*fromIntegral i

-- same as bgTileIndexToAddr but for OBJs
objTileIndexToAddr :: Int -> Int
objTileIndexToAddr i = objTileBufferAddress + 16*fromIntegral i


-- GHC is much better at opimising loops like this:
--
-- > doFromTo 0 255 $ \y ->
-- >   doFromTo 0 255 $ \x -> do ...
--
-- Than it is at optimising loops like this:
--
-- > sequence_ [ do ...
-- >           | x <- [0..255]
-- >           , y <- [0..255] ]
--
-- The first kind of loop runs significantly faster (with GHC 6.2 and 6.4)

{-# INLINE doFromTo #-}
-- do the action for [from..to], ie it's inclusive.
doFromTo :: (Monad m) => Int -> Int -> (Int -> m ()) -> m ()
doFromTo from to action =
  let loop n | n > to   = return ()
             | otherwise = do action n
                              loop (n+1)
   in loop from
