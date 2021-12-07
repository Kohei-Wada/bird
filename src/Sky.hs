{-#LANGUAGE RecordWildCards #-}
module Sky where

import Options
import Utils

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy


data Sky = Sky 
    { _skyX   :: Float 
    , _skyY   :: Float
    , _skyPic :: Picture
    , _skyWid :: Int
    }


skyInit :: IO Sky
skyInit = do 
    ps <- loadPictures __skyAssets
    
    let p = head ps
        (w, _) = pictureSize $ head ps

    return Sky { _skyX = 0.0 
               , _skyY = -150.0
               , _skyPic = pictures [p, translate (fromIntegral w) 0 p] 
               , _skyWid = w
               }


skyUpdate :: Sky -> Sky
skyUpdate s@Sky{..} = updateSkyPic s


updateSkyPic :: Sky -> Sky
updateSkyPic s@Sky{..} = 
    let x = if abs _skyX > fromIntegral _skyWid then 0 else _skyX - 0.5 
        in s { _skyX = x }

