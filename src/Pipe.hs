{-#LANGUAGE RecordWildCards #-}
module Pipe where

import Options
import Utils
import Bird
import Actor
import Control.Monad

data Pipe = Pipe 
    { _pipeUp    :: !Float
    , _pipeDw    :: !Float
    , _pipeX     :: !Float
    } deriving Show

newtype Pipes = Pipes [Pipe]


unPipes :: Pipes -> [Pipe] 
unPipes (Pipes ps) = ps


pipesInit :: IO [Pipe]
pipesInit = forM [1..__nPipes] $ \x -> do 
    let tmp = fromIntegral __wWidth 
    pipeInit $ (fromIntegral x * tmp / fromIntegral __nPipes) + tmp / 3.0


resetPipes :: [Pipe] -> IO [Pipe]
resetPipes ps = forM (zip ps [1..]) $ \(p, n) -> do 
    let tmp = fromIntegral __wWidth 
    pipeReset p (fromIntegral n * (tmp / fromIntegral (length ps)) + tmp / 3.0) 


pipeInit :: Float -> IO Pipe
pipeInit x = do
    r <- randomHeight 
    pure Pipe { _pipeUp    = r 
              , _pipeX     = x
              , _pipeDw    = r + __pipesGap 
              }


pipeReset :: Pipe -> Float -> IO Pipe
pipeReset p@Pipe{..} x =  do 
    r <- randomHeight
    pure p { _pipeX  = x 
           , _pipeUp = r
           , _pipeDw = r + __pipesGap 
           }


pipeUpdate :: Pipe -> IO Pipe
pipeUpdate p@Pipe{..} = 
    if _pipeX < -__wWidth / 2 
       then pipeReset p $  __wWidth / 2 
       else pure p { _pipeX = _pipeX + (__pipeSpeed / __fFps) }


pipeCollision :: Pipe -> Bird -> Bool
pipeCollision p@Pipe{..} b@Bird{..} = 
       _birdX <= _pipeX + fromIntegral __pipeWid__
    && _birdX >= _pipeX - fromIntegral __pipeWid__

    && ( _birdY + fromIntegral __birdHgt__ >= _pipeUp || 
         _birdY - fromIntegral __birdHgt__ <= _pipeDw
       ) 


insidePipeGap :: Pipe -> Bird -> Bool
insidePipeGap p@Pipe{..} b@Bird{..} =
       _birdX <= _pipeX + fromIntegral __pipeWid__ 
    && _birdX >= _pipeX - fromIntegral __pipeWid__ 
