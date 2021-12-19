{-#LANGUAGE RecordWildCards #-}
module Score where

import Bird 
import Pipe
import Options

import Graphics.Gloss


data Score = Score 
    { _num       :: Int 
    , _scoreX    :: Float
    , _scoreY    :: Float
    , _sFlag     :: Bool
    }


scoreInit :: Score 
scoreInit = Score 
    { _num       = 0 
    , _scoreX    = 0 
    , _scoreY    = __wHeight / 3
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
     in if _sFlag 
           then if f then s else addScore s
           else scoreSetFlag s f
