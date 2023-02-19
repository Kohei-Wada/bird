{-#LANGUAGE RecordWildCards #-}
module Pipe (Pipe(..), insidePipeGap, pipeCollision, pipesInit, pipeUpdate) where

import Options
import Bird
import Actor
import Control.Monad
import System.Random

import Control.Monad
import Control.Monad.ST
import Data.STRef

data Pipe = Pipe 
    { _pipeUp    :: !Float
    , _pipeDw    :: !Float
    , _pipeX     :: !Float
    } 

newtype Pipes = Pipes [Pipe]

unPipes :: Pipes -> [Pipe] 
unPipes (Pipes ps) = ps


pipesInit :: IO [Pipe]
pipesInit = forM [1..__nPipes] $ \x -> do 
    let tmp = fromIntegral __wWidth 
    pipeInit $ (fromIntegral x * tmp / fromIntegral __nPipes) + tmp / 3.0


pipeInit :: Float -> IO Pipe
pipeInit x = do
    r <- randomHeight 
    pure Pipe { _pipeUp    = r 
              , _pipeX     = x
              , _pipeDw    = r + __pipesGap 
              }


pipeUpdate :: Pipe -> IO Pipe
pipeUpdate p@Pipe{..} = 
    if _pipeX < -__wWidth / 2 
       then pipeInit (__wWidth / 2)
       else pure p { _pipeX = _pipeX + (__pipeSpeed / __fFps) }


pipeCollision :: Pipe -> Bird -> Bool
pipeCollision Pipe{..} Bird{..} = 
       _birdX <= _pipeX + fromIntegral __pipeWid__
    && _birdX >= _pipeX - fromIntegral __pipeWid__

    && ( _birdY + fromIntegral __birdHgt__ >= _pipeUp || 
         _birdY - fromIntegral __birdHgt__ <= _pipeDw
       ) 

randomHeight :: IO Float 
randomHeight = randomRIO( 0, __wHeight / 2) 


insidePipeGap :: Pipe -> Bird -> Bool
insidePipeGap Pipe{..} Bird{..} =
       _birdX <= _pipeX + fromIntegral __pipeWid__ 
    && _birdX >= _pipeX - fromIntegral __pipeWid__ 
