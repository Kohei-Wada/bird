module Utils where

import Options

import Control.Monad
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Juicy


loadPictures :: [FilePath] -> IO [Picture]
loadPictures ps = forM ps $ \s -> do 
    tmp <- loadJuicy s
    case tmp of 
      Just p -> pure p 


pictureSize :: Picture -> (Int, Int)
pictureSize p = let Bitmap b = p in bitmapSize b


makeLongPicW :: Picture -> Int -> Int -> Picture
makeLongPicW p r originW = 
    pictures [ translate (fromIntegral (a * originW)) 0 p | a <- [0..(r-1)] ]


expansionRate :: Int -> Int
expansionRate origin = round $ (__wWidth) / (fromIntegral origin) * 2 


digs :: Int -> [Int]
digs 0 = []
digs x = x `mod` 10 : digs (x `div` 10) 
