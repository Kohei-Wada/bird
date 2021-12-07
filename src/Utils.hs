module Utils where

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

