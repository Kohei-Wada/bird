{-#LANGUAGE RecordWildCards #-}
module Score where

    
import Utils
import Options

import Graphics.Gloss



data Score = Score 
    { _num       :: Int 
    , _scoreX    :: Float
    , _scoreY    :: Float
    , _scorePics :: [Picture] 
    }



scoreInit :: IO Score 
scoreInit = do 
    ps <- loadPictures __scoreAssets 
    return Score 
        { _num       = 0
        , _scoreX    = 0 
        , _scoreY    = __wHeight / 3
        , _scorePics = ps
        }


addScore :: Score -> Score
addScore s@Score{..} = s { _num = _num + 1 }


resetScore :: Score -> Score
resetScore s@Score{..} = s { _num = 0 }


-- TODO 
scorePicture :: Score -> Picture
scorePicture s@Score{..} = 
    translate  _scoreX _scoreY $ last _scorePics 

