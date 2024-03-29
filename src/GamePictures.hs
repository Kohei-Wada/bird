{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE BangPatterns #-}

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
    { _birdPics    :: ![Picture]
    , _groundPic   :: !Picture
    , _skyPic      :: !Picture
    , _pipePicUp   :: !Picture
    , _pipePicDw   :: !Picture
    , _pipePicLng  :: !Picture
    , _scorePics   :: ![Picture]
    , _logoPic     :: !Picture
    , _gameOverPic :: !Picture
    }


loadAllPictures :: IO GamePictures
loadAllPictures = do 
    bps <- loadPictures __birdAssets 
    gps <- loadPictures __groundAssets 
    sps <- loadPictures __skyAssets  
    pps <- loadPictures __pipeAssets 
    scs <- loadPictures __scoreAssets 
    lps <- loadPictures __logoAssets 
    gop <- loadPictures __gameOver

    let gp = head gps
        gr = expansionRate __groundWid__ 

        sp = head sps
        sr = expansionRate __skyWid__ 

        pp = pps !! 2

    return GamePictures 
        { _birdPics    = bps
        , _groundPic   = makeLongPicW gp gr __groundWid__ 
        , _skyPic      = makeLongPicW sp sr __skyWid__     
        , _pipePicUp   = pps !! 0 
        , _pipePicDw   = pps !! 1
        , _pipePicLng  = scale 1 (realToFrac __wHeight) pp 
        , _scorePics   = scs
        , _logoPic     = lps !! 0
        , _gameOverPic = gop !! 0 
        }

logoPicture :: GamePictures -> Picture 
logoPicture GamePictures{..} = translate 0 0 _logoPic 
{-# INLINE logoPicture #-}

gameOverPicture :: GamePictures -> Picture
gameOverPicture GamePictures{..} = translate 0 0 _gameOverPic 
{-# INLINE gameOverPicture #-}

birdPicture :: GamePictures -> Bird -> Picture
birdPicture GamePictures{..} Bird{..} = 
   translateD _birdX _birdY $ rotateD _angle (_birdPics !! _pIndex) 
{-# INLINE birdPicture #-}

groundPicture :: GamePictures -> Ground -> Picture 
groundPicture GamePictures{..} Ground{..} = translateD _groundX _groundY _groundPic 
{-# INLINE groundPicture #-}

skyPicture :: GamePictures -> Sky -> Picture
skyPicture GamePictures{..} Sky{..} = translateD _skyX _skyY _skyPic 
{-# INLINE skyPicture #-}

pipesPicture :: GamePictures -> Pipes -> [Picture]
pipesPicture gp (Pipes ps) = map (pipePicture gp) ps
{-# INLINE pipesPicture #-}

highScorePicture :: GamePictures -> Int -> Picture
highScorePicture _ n = 
    translateD (__wWidth / 3) __defaultGroundY $ scale 0.1 0.1 $ text $ "HIGH SCORE : " ++ show n 
{-# INLINE highScorePicture #-}

pipePicture :: GamePictures -> Pipe -> Picture
pipePicture GamePictures{..} Pipe{..} = 
     pictures [ translateD _pipeX (_pipeUp + __wHeight / 2) _pipePicLng
              , translateD _pipeX (_pipeDw - __wHeight / 2) _pipePicLng
              , translateD _pipeX _pipeUp _pipePicUp 
              , translateD _pipeX _pipeDw _pipePicDw
              ] 
{-# INLINE pipePicture #-}

-- TODO 
scorePicture :: GamePictures -> Score -> Picture
scorePicture GamePictures{..} Score{..} =
    if _value == 0 
       then translateD _scoreX _scoreY $ _scorePics !! 0
       else 
       let !nl = zip [0..] $ digs _value
           !ps = map (\(i, n) -> translateD (-__scoreWid__ * fromIntegral i) 0 (_scorePics !! n)) nl
        in translateD _scoreX _scoreY $ pictures ps 
{-# INLINE scorePicture #-}

-- utility function for translate
translateD :: Double -> Double -> Picture -> Picture
translateD x y p = translate (realToFrac x) (realToFrac y) p
{-# INLINE translateD #-}

-- utility function for rotate
rotateD :: Double -> Picture -> Picture
rotateD a p = rotate (realToFrac a) p
{-# INLINE rotateD #-}

