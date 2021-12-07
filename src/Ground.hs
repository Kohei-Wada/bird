{-#LANGUAGE RecordWildCards #-}
module Ground where


import Options
import Utils

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy


data Ground = Ground 
    { _groundX   :: Float 
    , _groundY   :: Float
    , _groundPic :: Picture
    , _groundWid :: Int
    }


groundInit :: IO Ground
groundInit = do 
    ps <- loadPictures __groundAssets
    let p = head ps
        (w, _) = pictureSize p
    
    return Ground 
        { _groundX   = 0.0 
        , _groundY   = -250.0 
        , _groundPic = pictures [ p, translate (fromIntegral w) 0 p ] 
        , _groundWid = w
        }


groundUpdate :: Ground -> Ground
groundUpdate g = updateGroundPic g


updateGroundPic :: Ground -> Ground
updateGroundPic g@Ground{..} =
    let x = if abs _groundX > fromIntegral _groundWid then 0 else _groundX - 1 
        in g { _groundX = x }



