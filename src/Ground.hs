{-#LANGUAGE RecordWildCards #-}
module Ground where


import Options
import Utils

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy


data Ground = Ground 
    { _groundX   :: !Float 
    , _groundY   :: !Float
    , _groundPic :: !Picture
    , _groundWid :: !Int
    }


groundInit :: IO Ground
groundInit = do 
    ps <- loadPictures __groundAssets
    let p    = head ps
        r = expansionRate __defaultGroundWid
        gWidth = __defaultGroundWid * r
    
    return Ground 
        { _groundX   = -fromIntegral gWidth 
        , _groundY   = __defaultGroundY 
        , _groundPic = makeLongPic p r __defaultGroundWid 
        , _groundWid = gWidth
        }


groundUpdate :: Ground -> Ground
groundUpdate g = updateGroundPic g


updateGroundPic :: Ground -> Ground
updateGroundPic g@Ground{..} =
    let x = if abs _groundX > (fromIntegral _groundWid) / 2
               then -fromIntegral __groundWid 
               else _groundX + __groundSpeed * (1.0 / __fFps)
     in g { _groundX = x }
