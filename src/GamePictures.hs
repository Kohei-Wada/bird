{-# LANGUAGE RecordWildCards #-}

module GamePictures where


import Bird
import Ground
import Sky
import Pipe
import Utils
import Options
import Score

import Graphics.Gloss


data GamePictures = GamePictures
    { _birdPics   :: [Picture]
    , _groundPic  :: Picture
    , _skyPic     :: Picture
    , _pipePicUp  :: Picture
    , _pipePicDw  :: Picture
    , _pipePicLng :: Picture
    , _scorePics  :: [Picture]
    }


loadAllPictures :: IO GamePictures
loadAllPictures = do 
    bps <- loadPictures __birdAssets 
    gps <- loadPictures __groundAssets 
    sps <- loadPictures __skyAssets  
    pps <- loadPictures __pipeAssets 
    scs <- loadPictures __scoreAssets 

    let gp = head gps
        gr = expansionRate __groundWid__ 

    let sp = head sps
        sr = expansionRate __skyWid__ 

    let pp = pps !! 2

    return GamePictures 
        { _birdPics   = bps
        , _groundPic  = makeLongPicW gp gr __groundWid__ 
        , _skyPic     = makeLongPicW sp sr __skyWid__ 
        , _pipePicUp  = pps !! 0 
        , _pipePicDw  = pps !! 1
        , _pipePicLng = makeLongPicH pp __wHeight __pipeHgt 
        , _scorePics  = scs
        }


birdPicture :: GamePictures -> Bird -> Picture
birdPicture gp@GamePictures{..} b@Bird{..} = 
   translate _birdX _birdY $ rotate _angle (_birdPics !! _pIndex) 


groundPicture :: GamePictures -> Ground -> Picture 
groundPicture gp@GamePictures{..} g@Ground{..} = 
    translate _groundX _groundY _groundPic 


skyPicture :: GamePictures -> Sky -> Picture
skyPicture gp@GamePictures{..} s@Sky{..} = translate _skyX _skyY _skyPic 


pipesPicture :: GamePictures -> [Pipe] -> [Picture]
pipesPicture gp = map $ pipePicture gp


pipePicture :: GamePictures -> Pipe -> Picture
pipePicture gp@GamePictures{..} p@Pipe{..} = 
     pictures [ translate _pipeX _pipeUp _pipePicLng
              , translate _pipeX (_pipeDw - __winHeight__ ) _pipePicLng
              , translate _pipeX _pipeUp _pipePicUp 
              , translate _pipeX _pipeDw _pipePicDw
              ] 

-- TODO 
scorePicture :: GamePictures -> Score -> Picture
scorePicture gp@GamePictures{..} s@Score{..} =
    if _num == 0 
       then translate _scoreX _scoreY $ _scorePics !! 0
       else 
       let nl = zip [0..] $ digs _num
           ps = map (\(i, n) -> translate (-__scoreWid__ * fromIntegral i) 0 (_scorePics !! n)) nl
        in translate _scoreX _scoreY $ pictures ps 

