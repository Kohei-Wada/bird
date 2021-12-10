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
        r = expansionRate __defaultSkyWid
        sWidth = __defaultSkyWid * r 
 
    return Sky { _skyX   = -fromIntegral sWidth 
               , _skyY   = __defaultSkyY 
               , _skyPic = makeLongPicW p r __defaultSkyWid
               , _skyWid = sWidth 
               }


skyUpdate :: Sky -> Sky
skyUpdate s = updateSkyX s


updateSkyX :: Sky -> Sky
updateSkyX s@Sky{..} = 
    let x = if abs _skyX > (fromIntegral _skyWid) / 2
               then -fromIntegral __skyWid else _skyX + __skySpeed * (1.0 / __fFps) 
     in s { _skyX = x }

