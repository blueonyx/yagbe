{-# LANGUAGE NoImplicitPrelude #-}
module Bitmap 
  ( bitmapToTiles
  ) where

import Prelude (print,putStrLn)
import RIO
import RIO.List (splitAt,transpose)
import RIO.List.Partial (init,tail)
import RIO.Prelude.Types (Word8)
import qualified RIO.ByteString.Lazy as LBS

-- read BMP with 32x32 pixel, 24bpp, 54byte header and bottom-up-bitmap
-- converts colors to palette indices:
--   ff ff ff = 0
--   c0 c0 c0 = 1
--   80 80 80 = 2
--   00 00 00 = 3
-- prints part of tiles array to stdout
--
-- NOTE: probably need to remove the very last comma?
bitmapToTiles :: FilePath -> IO ()
bitmapToTiles file = do
  rs <- concat . map transpose . tileMapRows . -- split again and reorder
        map (tileMapRows.toIndices) . -- convert colors to indices and split into 8ths
        reverse . bitmapRows . -- reververse the bottom up bitmap
        drop 54 . -- skip bitmap header
        LBS.unpack <$> LBS.readFile file
  mapM_ (\x -> mapM_ (putStrLn . ("  "++) . (++",") . concat . map show) x >> putStrLn "") rs
  return ()
  
-- splits into 32*3 chunks and even dont? reverses them (because of bottom-up-bitmap)
bitmapRows :: [Word8] -> [[Word8]]
bitmapRows = splitAllAt (32*3)

tileMapRows :: [a] -> [[a]]
tileMapRows = splitAllAt 8

-- splits the whole list into list of list of size num
splitAllAt :: Int -> [a] -> [[a]]
splitAllAt num = go []
  where go res [] = res
        go res xs = go (res++[a]) b
          where (a,b) = splitAt num xs
          
-- remove the 2nd and 3rd byte, replace with palette index, see above
toIndices :: [Word8] -> [Int]
toIndices = go []
  where go res [] = res
        go res (x:_:_:xs) = go (res++[toI x]) xs
        
        toI 255 = 0
        toI 192 = 1
        toI 128 = 2
        toI   0 = 3
