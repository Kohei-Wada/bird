{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE BangPatterns #-}
module Score where

import Bird 
import Pipe
import Options

import System.Directory
import System.IO 
import Control.Monad
import Control.Monad.ST
import Data.STRef


data Score = Score 
    { _value     :: !Int 
    , _scoreX    :: !Float
    , _scoreY    :: !Float
    , _sFlag     :: !Bool
    , _highScore :: !Int
    }


scoreInit :: IO Score 
scoreInit = do 
    hs <- loadhighScore 
    pure Score 
        { _value     = 0 
        , _scoreX    = 0 
        , _scoreY    = __wHeight / 3
        , _sFlag     = False
        , _highScore = hs
        }


addScore :: Score -> Score
addScore s@Score{..} = s { _value   = _value + 1 
                         , _sFlag = False
                         }

scoreSetFlag :: Score -> Bool -> Score 
scoreSetFlag s@Score{..} b = s { _sFlag = b }


updateScore :: Score -> Pipes -> Bird -> Score 
updateScore s@Score{..} ps b@Bird{..} = 
    let !f = insidePipesGap ps b
     in if _sFlag then if f then s else addScore s else scoreSetFlag s f


createFile :: FilePath -> IO () 
createFile p = do 
    handle <- openFile p WriteMode
    hClose handle
    

writeHighScore :: Score -> IO () 
writeHighScore Score{..} = do 
    f <- doesFileExist __scoreData__
    if f 
       then do 
       writeFile  __scoreData__ (show _value) 

       else do 
       createFile __scoreData__
       writeFile __scoreData__  (show _value)


loadhighScore :: IO Int
loadhighScore = do 
    f <- doesFileExist __scoreData__

    if f 
       then do 
       text <- readFile __scoreData__  
       pure $ read text 

       else 
       pure 0 
