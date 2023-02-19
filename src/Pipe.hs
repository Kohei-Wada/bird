{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE BangPatterns #-}
module Pipe 
    (
      Pipe(..)
    , Pipes(..)
    , pipesCollision
    , insidePipesGap
    ) where

import Options
import Bird
import Actor
import Control.Monad
import System.Random

import Control.Monad
import Control.Monad.ST
import Data.STRef

data Pipe = Pipe 
    { _pipeUp :: !Float
    , _pipeDw :: !Float
    , _pipeX  :: !Float
    } 

newtype Pipes = Pipes [Pipe]

instance Actor Pipes where
    initialize = pipesInit
    update     = pipesUpdate

randomHeight :: IO Float 
randomHeight = randomRIO( 0, __wHeight / 2) 


pipesInit :: IO Pipes
pipesInit = do 
    ps <- forM [1 .. __nPipes] $ \x -> do 
        r <- randomHeight
        pure $ pipeInit r ((fromIntegral x * fromIntegral __wWidth / fromIntegral __nPipes) + fromIntegral __wWidth / 3.0)
    pure $ Pipes ps 


pipeInit :: Float -> Float -> Pipe
pipeInit r x = 
    Pipe { _pipeUp    = r 
         , _pipeX     = x
         , _pipeDw    = r + __pipesGap 
         }


pipesUpdate :: Pipes -> IO Pipes
pipesUpdate (Pipes ps) = do 
    ps' <- forM ps $ \p@Pipe{..} -> do 
        if _pipeX < -__wWidth / 2 
           then do 
               r <- randomHeight
               pipeReset p (__wWidth / 2) r
           else do 
               pipeUpdate p

    pure $ Pipes ps'


pipeReset :: Pipe -> Float -> Float -> IO Pipe
pipeReset p@Pipe{..} x r = stToIO $ do 
    p' <- newSTRef p
    modifySTRef p' (\p -> p { _pipeX  = x, _pipeUp = r, _pipeDw = r + __pipesGap })
    readSTRef p'


pipeUpdate :: Pipe -> IO Pipe
pipeUpdate p = stToIO $ do 
    p' <- newSTRef p
    modifySTRef p' (\p@Pipe{..} -> p { _pipeX = _pipeX + (__pipeSpeed / __fFps) })
    readSTRef p'


pipesCollision :: Pipes -> Bird -> Bool
pipesCollision (Pipes ps) b = any (\p -> pipeCollision p b) ps


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
               _birdX <= _pipeX + fromIntegral __pipeWid__ 
            && _birdX >= _pipeX - fromIntegral __pipeWid__ 
