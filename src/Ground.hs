{-#LANGUAGE RecordWildCards #-}
module Ground where

import Options
import Utils

import Graphics.Gloss


data Ground = Ground 
    { _groundX   :: !Float 
    , _groundY   :: !Float
    , _groundPic :: !Picture
    , _groundWid :: !Int
    }


groundInit :: IO Ground
groundInit = do 
    ps <- loadPictures __groundAssets
    let p = head ps
        r = expansionRate __groundWid__
        gWidth = __groundWid__ * r
    
    return Ground 
        { _groundX   = -fromIntegral gWidth 
        , _groundY   = __defaultGroundY 
        , _groundPic = makeLongPicW p r __groundWid__ 
        , _groundWid = gWidth
        }


groundUpdate :: Ground -> Ground
groundUpdate g = updateGroundX g


updateGroundX :: Ground -> Ground
updateGroundX g@Ground{..} =
    g { _groundX = if _groundX < -(fromIntegral __wWidth)
               then -fromIntegral __groundWid 
               else _groundX + __groundSpeed * (1.0 / __fFps)
      }


