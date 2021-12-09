{-#LANGUAGE RecordWildCards #-}

module Pipe where

import Options
import Utils
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


data Pipe = Pipe 
    { _pipeUp   :: !Float
    , _pipeDown :: !Float
    , _pipeX    :: !Float
    , _pipePic  :: Picture
    } deriving Show



pipeInit :: IO Pipe
pipeInit = do
    ps <- loadPictures __pipeAssets

    return Pipe { _pipeUp = 100 
                , _pipeDown = 0.0
                , _pipeX = 0.0
                , _pipePic =  ps !! 0
                }




