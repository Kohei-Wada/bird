{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE BangPatterns #-}
module Score where

import Bird 
import Pipe
import Options

import System.IO
import System.Directory
import Control.Monad
import Data.Maybe (fromMaybe)

data Score = Score 
    { _value     :: !Int 
    , _scoreX    :: !Double
    , _scoreY    :: !Double
    , _sFlag     :: !Bool
    , _highScore :: !Int
    }

-- Pure function to parse high score from text
parseHighScore :: String -> Int
parseHighScore s = fromMaybe 0 (readMaybe s)

-- Pure function to prepare score data for writing
formatHighScore :: Score -> String
formatHighScore = show . _value

scoreInit :: IO Score 
scoreInit = do 
    hs <- loadHighScoreFromFile __scoreData__
    pure Score 
        { _value     = 0 
        , _scoreX    = 0 
        , _scoreY    = __wHeight / 3
        , _sFlag     = False
        , _highScore = hs
        }

addScore :: Score -> Score
addScore s@Score{..} = s { _value   = _value + 1, _sFlag = False }

scoreSetFlag :: Score -> Bool -> Score 
scoreSetFlag s@Score{..} b = s { _sFlag = b }

updateScore :: Score -> Pipes -> Bird -> Score 
updateScore s@Score{..} ps b = 
    let !f = insidePipesGap ps b
     in if _sFlag then if f then s else addScore s else scoreSetFlag s f

-- Impure function for writing high score
writeHighScoreToFile :: FilePath -> Score -> IO ()
writeHighScoreToFile path score = do
    let scoreStr = formatHighScore score
    safeWriteFile path scoreStr

-- Impure function for loading high score
loadHighScoreFromFile :: FilePath -> IO Int
loadHighScoreFromFile path = do
    fileExists <- doesFileExist path
    if fileExists
        then do
            text <- readFile path
            pure $ parseHighScore text
        else pure 0

-- Helper to read a value safely
readMaybe :: Read a => String -> Maybe a
readMaybe s = case reads s of
    [(val, "")] -> Just val
    _           -> Nothing

-- Helper to write to a file safely, creating it if it doesn't exist
safeWriteFile :: FilePath -> String -> IO ()
safeWriteFile path content = withFile path WriteMode (\h -> hPutStr h content) 
