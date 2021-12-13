{-#LANGUAGE RecordWildCards #-}

module Pipe where

import Options
import Utils

import Control.Monad
import Graphics.Gloss


data Pipe = Pipe 
    { _pipeUp    :: !Float
    , _pipeDw    :: !Float
    , _pipeX     :: !Float
    , _pipePicUp :: !Picture
    , _pipePicDw :: !Picture
    , _pipePic   :: !Picture
    } deriving Show


-----------------------------------------------------------------------------------------


pipesInit :: Int -> IO [Pipe]
pipesInit n = forM [1..n] $ \x -> do 
     pipeInit (fromIntegral x * fromIntegral __wWidth / fromIntegral n)


pipesUpdate :: [Pipe] -> IO [Pipe]
pipesUpdate ps = forM ps pipeUpdate 


pipesCollision :: [Pipe] -> Float -> Float -> Bool
pipesCollision ps x y = any (\p -> pipeCollision p x y) ps


resetPipes :: [Pipe] -> IO [Pipe]
resetPipes ps = forM (zip ps [1..]) $ \(p, n) ->
    pipeReset p (fromIntegral n * fromIntegral __wWidth / fromIntegral (length ps)) 


-----------------------------------------------------------------------------------------


pipeInit :: Float -> IO Pipe
pipeInit x = do
    ps <- loadPictures __pipeAssets
    r  <- randomHeight 

    return Pipe { _pipeUp    = r 
                , _pipeDw    = r + __pipesGap 
                , _pipeX     = x
                , _pipePicUp = ps !! 0
                , _pipePicDw = ps !! 1
                , _pipePic   = ps !! 2
                }


pipeReset :: Pipe -> Float -> IO Pipe
pipeReset p@Pipe{..} x =  do 
    r <- randomHeight
    return p { _pipeX  = x 
             , _pipeUp = r
             , _pipeDw = r + __pipesGap 
             }



newPipe :: Pipe -> IO Pipe
newPipe p@Pipe{..} = do 
    r <- randomHeight 
    return p { _pipeX  = __wWidth / 2  
             , _pipeUp = r
             , _pipeDw = r + __pipesGap 
             }


pipeUpdate :: Pipe -> IO Pipe
pipeUpdate p@Pipe{..} = 
    if _pipeX < -__wWidth / 2 
       then newPipe p
       else return p { _pipeX = _pipeX + (__pipeSpeed / __fFps) }


-- TODO 
pipeCollision :: Pipe -> Float -> Float -> Bool
pipeCollision p@Pipe{..} x y = False 


