{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE BangPatterns #-}
module Pipe 
    (
      Pipe(..)
    , Pipes(..)
    , pipesCollision
    , insidePipesGap
    , pipeUpdate
    , pipeReset
    ) where

import Options
import Bird
import Actor
import Control.Monad
import System.Random

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Data.Array.ST
import GHC.IO 

newtype Pipes = Pipes [Pipe]

data Pipe = Pipe 
    { _pipeUp :: !Double
    , _pipeDw :: !Double
    , _pipeX  :: !Double
    } 

instance Actor Pipes where
    initialize  = pipesInit
    update      = pipesUpdate
    onCollision = undefined

randomHeight :: IO Double
randomHeight = randomRIO( 0, __wHeight / 2) 

pipesInit :: IO Pipes
pipesInit = do 
    ps <- forM [1 .. __nPipes] $ \x -> do 
        r <- randomHeight
        pure $ pipeInit r ((fromIntegral x * __wWidth / fromIntegral __nPipes) + __wWidth / 3.0)
    pure $ Pipes ps 


pipeInit :: Double -> Double -> Pipe
pipeInit r x = 
    Pipe { _pipeUp    = r 
         , _pipeX     = x
         , _pipeDw    = r + __pipesGap 
         }

pipesUpdate :: Pipes -> IO Pipes
pipesUpdate (Pipes ps) = do
    newPs <- forM ps $ \p -> do
        if _pipeX p < -__wWidth / 2
            then do
                r <- randomHeight
                pure $ pipeReset p (__wWidth / 2) r
            else do
                pure $ pipeUpdate p
    pure $ Pipes newPs

pipeReset :: Pipe -> Double -> Double -> Pipe
pipeReset p@Pipe{..} x r = p { _pipeX  = x, _pipeUp = r, _pipeDw = r + __pipesGap }

pipeUpdate :: Pipe -> Pipe
pipeUpdate p@Pipe{..} = p { _pipeX = _pipeX + (__pipeSpeed / __dFps) }

pipesCollision :: Pipes -> Bird -> Bool
pipesCollision (Pipes ps) b = any (\p -> pipeCollision p b) ps 
    where
        pipeCollision :: Pipe -> Bird -> Bool
        pipeCollision Pipe{..} Bird{..} = 
               _birdX <= _pipeX + fromIntegral __pipeWid__
            && _birdX >= _pipeX - fromIntegral __pipeWid__

            && ( _birdY + fromIntegral __birdHgt__ >= _pipeUp || 
                 _birdY - fromIntegral __birdHgt__ <= _pipeDw
               ) 


insidePipesGap :: Pipes -> Bird -> Bool
insidePipesGap (Pipes ps) b = any (\p -> insidePipeGap p b) ps 
    where
        insidePipeGap :: Pipe -> Bird -> Bool
        insidePipeGap Pipe{..} Bird{..} =
               _birdX <= _pipeX + fromIntegral __pipeWid__ && _birdX >= _pipeX - fromIntegral __pipeWid__