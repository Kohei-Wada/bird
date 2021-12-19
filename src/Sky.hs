{-#LANGUAGE RecordWildCards #-}
module Sky where

import Options
import Utils

import Graphics.Gloss


data Sky = Sky 
    { _skyX   :: !Float 
    , _skyY   :: !Float
    , _skyWid :: !Int
    }


skyInit :: Sky
skyInit = let w = __skyWid__ * expansionRate __skyWid__  
           in Sky 
               { _skyX   = -fromIntegral w 
               , _skyY   = __defaultSkyY 
               , _skyWid = w 
               }


skyUpdate :: Sky -> Sky
skyUpdate = updateSkyX 


updateSkyX :: Sky -> Sky
updateSkyX s@Sky{..} = 
    s { _skyX  = if _skyX <  - (fromIntegral __wWidth) 
                    then -fromIntegral __skyWid 
                    else _skyX + __skySpeed * (1.0 / __fFps) 
      }

