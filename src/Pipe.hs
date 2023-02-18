{-#LANGUAGE RecordWildCards #-}

module Pipe where

import Options
import Utils
import Actor

import Control.Monad


data Pipe = Pipe 
    { _pipeUp    :: !Float
    , _pipeDw    :: !Float
    , _pipeX     :: !Float
    } deriving Show


instance Actor Pipe where
    update = pipeUpdate


pipesInit :: Int -> IO [Pipe]
pipesInit n = forM [1..n] $ \x -> do 
    let tmp = fromIntegral __wWidth 
    pipeInit $ (fromIntegral x * tmp / fromIntegral n) + tmp / fromIntegral 3


pipesCollision :: [Pipe] -> Float -> Float -> Bool
pipesCollision ps x y = any (\p -> pipeCollision p x y) ps


resetPipes :: [Pipe] -> IO [Pipe]
resetPipes ps = forM (zip ps [1..]) $ \(p, n) -> do 
    let tmp = fromIntegral __wWidth 
    pipeReset p (fromIntegral n * (tmp / fromIntegral (length ps)) + tmp / fromIntegral 3) 


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


pipeCollision :: Pipe -> Float -> Float -> Bool
pipeCollision p@Pipe{..} x y = 
       x <= _pipeX + fromIntegral __pipeWid__
    && x >= _pipeX - fromIntegral __pipeWid__ 
     
    && ( y + fromIntegral __birdHgt__ >= _pipeUp || 
         y - fromIntegral __birdHgt__ <= _pipeDw  
       ) 


insidePipeGap :: Pipe -> Float -> Bool
insidePipeGap p@Pipe{..} x = 
       x <= _pipeX + fromIntegral __pipeWid__ 
    && x >= _pipeX - fromIntegral __pipeWid__ 
