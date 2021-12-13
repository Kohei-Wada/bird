{-#LANGUAGE RecordWildCards #-}
module Ground where

import Options
import Utils

import Graphics.Gloss


data Ground = Ground 
    { _groundX   :: !Float 
    , _groundY   :: !Float
    , _groundWid :: !Int
    , _groundPic :: Picture
    }


groundInit :: IO Ground
groundInit = do 
    ps <- loadPictures __groundAssets
    let p = head ps
        r = expansionRate __groundWid__
        w = __groundWid__ * r
    
    return Ground 
        { _groundX   = -fromIntegral w 
        , _groundY   = __defaultGroundY 
        , _groundWid = w
        , _groundPic = makeLongPicW p r __groundWid__ 
        }


groundUpdate :: Ground -> Ground
groundUpdate = updateGroundX


updateGroundX :: Ground -> Ground
updateGroundX g@Ground{..} =
    g { _groundX = if _groundX < -(fromIntegral __wWidth)
               then -fromIntegral __groundWid 
               else _groundX + __groundSpeed * (1.0 / __fFps)
      }


-- TODO
groundCollision :: Ground -> Float -> Float -> Bool
groundCollision g@Ground{..} x y = y < _groundY + 80 

