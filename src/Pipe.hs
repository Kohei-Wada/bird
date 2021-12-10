{-#LANGUAGE RecordWildCards #-}

module Pipe where

import Options
import Utils
import Control.Monad
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


data Pipe = Pipe 
    { _pipeUp    :: !Float
    , _pipeDw    :: !Float
    , _pipeX     :: !Float
    , _pipePicUp :: !Picture
    , _pipePicDw :: !Picture
    , _pipePic   :: !Picture
    } deriving Show


-----------------------------------------------------------------------------------------
 
pipesInit :: IO [Pipe]
pipesInit = do return []


pipesUpdate ::[Pipe] -> IO [Pipe]
pipesUpdate ps = return ps

-----------------------------------------------------------------------------------------


pipeInit :: Float -> IO Pipe
pipeInit x = do
    ps <- loadPictures __pipeAssets
    r <- randomHeight 

    return Pipe { _pipeUp    = r 
                , _pipeDw    = r + __pipesGap 
                , _pipeX     = x
                , _pipePicUp = ps !! 0
                , _pipePicDw = ps !! 1
                , _pipePic   = ps !! 2
                }


newPipe :: Pipe -> IO Pipe
newPipe p@Pipe{..} = do 
    r <- randomHeight 
    return p { _pipeX = __wWidth  
             , _pipeUp = r
             , _pipeDw = r + __pipesGap 
             }


pipeUpdate :: Pipe -> IO Pipe
pipeUpdate p@Pipe{..} = 
    if _pipeX < -__wWidth / 2 
       then newPipe p
       else return p { _pipeX = _pipeX + (__pipeSpeed / __fFps) }


