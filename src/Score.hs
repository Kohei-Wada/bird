{-#LANGUAGE RecordWildCards #-}
module Score where

import Bird 
import Pipe
    
import Utils
import Options

import Graphics.Gloss


data Score = Score 
    { _num       :: Int 
    , _scoreX    :: Float
    , _scoreY    :: Float
    , _scorePics :: [Picture] 
    , _sFlag     :: Bool
    }


scoreInit :: IO Score 
scoreInit = do 
    ps <- loadPictures __scoreAssets 
    return Score 
        { _num       = 0 
        , _scoreX    = 0 
        , _scoreY    = __wHeight / 3
        , _scorePics = ps
        , _sFlag     = False
        }


addScore :: Score -> Score
addScore s@Score{..} = s { _num   = _num + 1 
                         , _sFlag = False
                         }

scoreSetFlag :: Score -> Bool -> Score 
scoreSetFlag s@Score{..} b = s { _sFlag = b }


scoreReset :: Score -> Score
scoreReset s@Score{..} = s { _num = 0 
                           , _sFlag = False 
                           }


updateScore :: Score -> [Pipe] -> Bird -> Score 
updateScore s@Score{..} ps b@Bird{..} = 
    let f = any (\p -> insidePipeGap p _birdX) ps 
     in  if _sFlag 
               then if f then s else addScore s
               else scoreSetFlag s f


scorePicture :: Score -> Picture
scorePicture s@Score{..} =
    if _num == 0 
       then translate _scoreX _scoreY $ _scorePics !! 0
       else 
        let nl = zip [0..] $ digs _num
            ps = map (\(i, n) -> translate (-__scoreWid__ * fromIntegral i) 0 (_scorePics !! n)) nl
         in translate _scoreX _scoreY $ pictures ps 



