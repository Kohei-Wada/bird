{-#LANGUAGE RecordWildCards #-}
module Sky where

import Options
import Utils

import Graphics.Gloss


data Sky = Sky 
    { _skyX   :: !Float 
    , _skyY   :: !Float
    , _skyWid :: !Int
    , _skyPic ::  Picture
    }


skyInit :: IO Sky
skyInit = do 
    ps <- loadPictures __skyAssets
    let p = head ps
        r = expansionRate __skyWid__  
        w = __skyWid__ * r
 
    return Sky 
        { _skyX   = -fromIntegral w 
        , _skyY   = __defaultSkyY 
        , _skyWid = w 
        , _skyPic = makeLongPicW p r __skyWid__
        }


skyUpdate :: Sky -> Sky
skyUpdate = updateSkyX 


updateSkyX :: Sky -> Sky
updateSkyX s@Sky{..} = 
    s { _skyX  = if _skyX <  - (fromIntegral __wWidth) 
                    then -fromIntegral __skyWid 
                    else _skyX + __skySpeed * (1.0 / __fFps) 
      }
