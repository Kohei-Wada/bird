{-#LANGUAGE RecordWildCards #-}

module Bird where
    
import Utils
import Options

import Data.Maybe    
import Graphics.Gloss.Juicy
import Graphics.Gloss


data Bird = Bird 
    { _birdX    :: Float
    , _birdY    :: Float 
    , _birdVy   :: Float     
    , _birdPic  :: Picture   -- Current bird Picture
    , _birdPics :: [Picture] -- All bird Pictures
    , _count    :: Int       -- count for FPS
    , _pIndex   :: Int       -- Picture Index
    } deriving Show


birdInit :: IO Bird
birdInit = do 
    ps <- loadPictures __birdAssets
    return Bird { _birdX     = (-1) * __wWidth / 3
                , _birdY     = __wHeight / 2 
                , _birdVy    = 0 
                , _birdPic   = head ps
                , _birdPics  = ps
                , _count     = 0
                , _pIndex    = 0
                }

moveBirdX :: Bird -> Float -> Bird
moveBirdX b@Bird{..} dx = b { _birdX = _birdX + dx }


updateBirdY :: Bird -> Bird
updateBirdY b@Bird{..} = b { _birdY = _birdY - _birdVy * (1.0 / __fFps) } 


updateBirdVY :: Bird -> Bird
updateBirdVY b@Bird{..} = b { _birdVy = _birdVy + __gravity * (1.0 / __fFps) }

    
birdFalling :: Bird -> Bird   
birdFalling b@Bird{..} = (updateBirdY . updateBirdVY ) b


birdFlapping :: Bird -> Bird
birdFlapping b = b { _birdVy = __birdFlappingV }


birdUpdate :: Bird -> Bird
birdUpdate b = (updateBirdPic . birdFalling) b


--TODO 
calcrateAngle :: Float -> Float
calcrateAngle vy 
  | tmp > 90  = 90
  | tmp < -45 = -45
  | otherwise = tmp 
  where tmp = vy / 10


updateBirdPic :: Bird -> Bird
updateBirdPic b@Bird{..} = 
    let c = if fromIntegral _count >= (__fFps / __fPps) then 0 else _count + 1
        i = if c == 0 
               then if _pIndex == (length _birdPics - 1) then 0 else _pIndex + 1
               else _pIndex
        p = rotate (calcrateAngle _birdVy) (_birdPics !! i) 

     in b { _birdPic = p , _count = c , _pIndex = i}
