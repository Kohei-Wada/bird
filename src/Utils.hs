module Utils where


import Options

import Data.Maybe    
import Graphics.Gloss.Juicy
import Graphics.Gloss


loadPictures :: [FilePath] ->  IO [Picture]
loadPictures [] = return []
loadPictures (s:ss) = do 
    Just p <- loadJuicy s
    ps <- loadPictures ss
    return (p:ps)


pictureSize :: Picture -> (Int, Int)
pictureSize p = 
    let Bitmap bitmap = p 
     in bitmapSize bitmap


makeLongPicW :: Picture -> Int -> Int -> Picture
makeLongPicW p r originW = 
    pictures [ translate (fromIntegral (a * originW)) 0 p | a <- [0..(r-1)] ]


makeLongPicH :: Picture -> Int -> Int -> Picture
makeLongPicH p r originH = 
    pictures [ translate 0 (fromIntegral (a * originH)) p | a <- [0..(r-1)] ]


expansionRate :: Int -> Int
expansionRate origin = round $ (fromIntegral __wWidth) / (fromIntegral origin) * 3.0 + 1.0 

