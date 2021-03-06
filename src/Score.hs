{-#LANGUAGE RecordWildCards #-}
module Score where

import Bird 
import Pipe
import Options

import System.Directory
import System.IO 


data Score = Score 
    { _value     :: Int 
    , _scoreX    :: Float
    , _scoreY    :: Float
    , _sFlag     :: Bool
    }


scoreInit :: Score 
scoreInit = Score 
    { _value     = 0 
    , _scoreX    = 0 
    , _scoreY    = __wHeight / 3
    , _sFlag     = False
    }


addScore :: Score -> Score
addScore s@Score{..} = s { _value   = _value + 1 
                         , _sFlag = False
                         }

scoreSetFlag :: Score -> Bool -> Score 
scoreSetFlag s@Score{..} b = s { _sFlag = b }


scoreReset :: Score -> Score
scoreReset s@Score{..} = s { _value = 0 
                           , _sFlag = False 
                           }


updateScore :: Score -> [Pipe] -> Bird -> Score 
updateScore s@Score{..} ps b@Bird{..} = 
    let f = any (\p -> insidePipeGap p _birdX) ps 
     in if _sFlag then if f then s else addScore s else scoreSetFlag s f


createFile :: FilePath -> IO () 
createFile p = do 
    handle <- openFile p WriteMode
    hClose handle
    

writeHighScore :: Int -> IO () 
writeHighScore n = do 
    f <- doesFileExist __scoreData__

    if f 
       then do 
       writeFile  __scoreData__ (show n) 

       else do 
       createFile __scoreData__
       writeFile __scoreData__  (show n)


loadhighScore :: IO Int
loadhighScore = do 
    f <- doesFileExist __scoreData__

    if f 
       then do 
       text <- readFile __scoreData__  
       return $ read text :: IO Int

       else 
       return 0 
