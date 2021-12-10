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
    , _pipePicUp :: Picture
    , _pipePicDw :: Picture
    , _pipePic   :: Picture
    } deriving Show


pipeInit :: IO Pipe
pipeInit = do
    ps <- loadPictures __pipeAssets
    print $ pictureSize $ps !! 2

    return Pipe { _pipeUp    = __wHeight / 8 
                , _pipeDw    = - __wHeight / 8 
                , _pipeX     = __wWidth / 2 
                , _pipePicUp = ps !! 0
                , _pipePicDw = ps !! 1
                , _pipePic   = ps !! 2
                }


pipeUpdate :: Pipe -> Pipe
pipeUpdate p@Pipe{..} = updatePipeX p


updatePipeX :: Pipe -> Pipe
updatePipeX p@Pipe{..} = 
    let x = if _pipeX < -__wWidth / 2 then __wWidth else _pipeX + (__pipeSpeed / __fFps)
     in p { _pipeX = x }
