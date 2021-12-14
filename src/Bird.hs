{-#LANGUAGE RecordWildCards #-}

module Bird where
    
import Utils
import Options

import Graphics.Gloss


data Bird = Bird 
    { _birdX    :: !Float
    , _birdY    :: !Float 
    , _birdVx   ::  Float
    , _birdVy   :: !Float     
    , _birdPics :: ![Picture] -- TODO move _birdPics inside Game
    , _count    :: !Int       -- count for FPS
    , _pIndex   :: !Int       -- Picture Index
    , _angle    :: Float
    } 
    deriving (Show, Eq)


birdInit :: IO Bird
birdInit = do 
    ps <- loadPictures __birdAssets
    return Bird 
        { _birdX    = __birdX
        , _birdY    = __birdY 
        , _birdVx   = 0 
        , _birdVy   = 0 
        , _birdPics = ps
        , _count    = 0
        , _pIndex   = 0
        , _angle    = 0
        }


birdReset :: Bird -> Bird
birdReset b@Bird{..} = b { _birdX  = __birdX
                         , _birdY  = __birdY 
                         , _birdVx = 0 
                         , _birdVy = 0 
                         , _angle  = 0
                         }


setBirdVx :: Bird -> Float -> Bird
setBirdVx b@Bird{..} vX = b { _birdVx = vX }


setBirdVy :: Bird -> Float -> Bird
setBirdVy b@Bird{..} vY = b { _birdVy = vY }


updateBirdY :: Bird -> Bird
updateBirdY b@Bird{..} = b { _birdY = _birdY - _birdVy * (1.0 / __fFps) } 


updateBirdVy :: Bird -> Bird
updateBirdVy b@Bird{..} = b { _birdVy = _birdVy + __gravity * (1.0 / __fFps) }

    
birdFalling :: Bird -> Bird   
birdFalling = updateBirdY . updateBirdVy


birdFlapping :: Bird -> Bird
birdFlapping b = setBirdVy b __birdFlappingV 


birdUpdate :: Bird -> Bird
birdUpdate b = (updateAngle . updatePicIndex . updateCount . birdFalling) b


updateCount :: Bird -> Bird
updateCount b@Bird{..} = 
    b { _count = if (fromIntegral _count) >= (__fFps / __fPps) then 0 else _count + 1 }


updatePicIndex :: Bird -> Bird
updatePicIndex b@Bird{..} = 
    let i = if _count == 0 
               then if _pIndex == (length _birdPics - 1) 
                       then 0 
                       else _pIndex + 1
               else _pIndex
     in b { _pIndex = i }


updateAngle :: Bird -> Bird 
updateAngle b@Bird{..} = b { _angle = calcurateAngle _birdVy }


calcurateAngle :: Float -> Float
calcurateAngle vy 
  | tmp > __maxBirdAngle = __maxBirdAngle 
  | tmp < __minBirdAngle = __minBirdAngle
  | otherwise = tmp 
  where tmp = velocityToAngle vy


velocityToAngle :: Float -> Float
velocityToAngle v = v / __angleBias 


birdPicture :: Bird -> Picture
birdPicture b@Bird{..} = 
   translate _birdX _birdY $ rotate _angle (_birdPics !! _pIndex) 


