{-#LANGUAGE RecordWildCards #-}
module Sky where

import Options
import Utils

import Graphics.Gloss


data Sky = Sky 
    { _skyX   :: !Float 
    , _skyY   :: !Float
    , _skyPic :: !Picture
    , _skyWid :: !Int
    }


skyInit :: IO Sky
skyInit = do 
    ps <- loadPictures __skyAssets
    let p = head ps
        r = expansionRate __skyWid__  
        sWidth = __skyWid__ * r
 
    return Sky { _skyX   = -fromIntegral sWidth 
               , _skyY   = __defaultSkyY 
               , _skyPic = makeLongPicW p r __skyWid__
               , _skyWid = sWidth 
               }


skyUpdate :: Sky -> Sky
skyUpdate = updateSkyX 


updateSkyX :: Sky -> Sky
updateSkyX s@Sky{..} = 
    s { _skyX  = if _skyX <  - (fromIntegral __wWidth) 
                    then -fromIntegral __skyWid 
                    else _skyX + __skySpeed * (1.0 / __fFps) 
      }
