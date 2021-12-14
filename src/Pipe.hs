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

pipesPicture :: [Pipe] -> [Picture]
pipesPicture =  map pipePicture 


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


pipeCollision :: Pipe -> Float -> Float -> Bool
pipeCollision p@Pipe{..} x y = 
       x <= _pipeX + fromIntegral __pipeWid__
    && x >= _pipeX - fromIntegral __pipeWid__ 
     
    && ( y + fromIntegral __birdHgt__ >= _pipeUp || 
         y - fromIntegral __birdHgt__ <= _pipeDw  
       ) 


pipePicture :: Pipe -> Picture
pipePicture p@Pipe{..} = 
    let tmpUp = makeLongPicH _pipePic __wHeight __pipeHgt 
        tmpDw = makeLongPicH _pipePic __wHeight (- __pipeHgt)
     in pictures [ translate _pipeX _pipeUp tmpUp
                 , translate _pipeX _pipeDw tmpDw
                 , translate _pipeX _pipeUp _pipePicUp 
                 , translate _pipeX _pipeDw _pipePicDw
                 ] 


insidePipeGap :: Pipe -> Float -> Bool
insidePipeGap p@Pipe{..} x = 
       x <= _pipeX + fromIntegral __pipeWid__ 
    && x >= _pipeX - fromIntegral __pipeWid__ 
    
