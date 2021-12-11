module Utils where


import Options

import Data.Maybe    
import System.Random
import Graphics.Gloss.Juicy
import Graphics.Gloss
import Control.Monad


loadPictures :: [FilePath] -> IO [Picture]
loadPictures ss = forM ss $ \s -> do 
    Just p <- loadJuicy s
    return p


pictureSize :: Picture -> (Int, Int)
pictureSize p = let Bitmap b = p in bitmapSize b


makeLongPicW :: Picture -> Int -> Int -> Picture
makeLongPicW p r originW = 
    pictures [ translate (fromIntegral (a * originW)) 0 p | a <- [0..(r-1)] ]


makeLongPicH :: Picture -> Int -> Int -> Picture
makeLongPicH p r originH = 
    pictures [ translate 0 (fromIntegral (a * originH)) p | a <- [0..(r-1)] ]


expansionRate :: Int -> Int
expansionRate origin = round $ (fromIntegral __wWidth) / (fromIntegral origin) * 2 


randomHeight:: IO Float 
randomHeight = 
    randomRIO(0, __wHeight / 2) :: IO Float 
