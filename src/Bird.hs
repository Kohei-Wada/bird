{-#LANGUAGE RecordWildCards #-}

module Bird where
    
import Options

import Graphics.Gloss


data Bird = Bird 
    { _birdX    :: !Float
    , _birdY    :: !Float 
    , _birdVy   :: !Float     
    , _count    :: !Int       -- count for FPS
    , _pIndex   :: !Int       -- Picture Index
    , _angle    :: Float
    , _dead     :: Bool
    } 
    deriving (Show, Eq)


birdInit :: Bird
birdInit = Bird 
        { _birdX    = __birdX
        , _birdY    = __birdY 
        , _birdVy   = 0 
        , _count    = 0
        , _pIndex   = 0
        , _angle    = 0
        , _dead     = False
        }


birdReset :: Bird -> Bird
birdReset b@Bird{..} = b { _birdX  = __birdX
                         , _birdY  = __birdY 
                         , _birdVy = 0 
                         , _angle  = 0
                         , _dead   = False
                         }


--setBirdVx :: Bird -> Float -> Bird
--setBirdVx b@Bird{..} vX = b { _birdVx = vX }


setBirdVy :: Bird -> Float -> Bird
setBirdVy b@Bird{..} vY = b { _birdVy = vY }


setBirdDead :: Bird -> Bool -> Bird 
setBirdDead b@Bird{..} f = b { _dead = f }


updateBirdY :: Bird -> Bird
updateBirdY b@Bird{..} = b { _birdY = _birdY - _birdVy * (1.0 / __fFps) } 


updateBirdVy :: Bird -> Bird
updateBirdVy b@Bird{..} = b { _birdVy = _birdVy + __gravity * (1.0 / __fFps) }

    
birdFalling :: Bird -> Bird   
birdFalling = updateBirdY . updateBirdVy


birdFlapping :: Bird -> Bird
birdFlapping b@Bird{..} = if _dead then b else setBirdVy b __birdFlappingV 


birdSwooping :: Bird -> Bird
birdSwooping b@Bird{..} = if _dead then b else setBirdVy b __birdSwoopingV


birdUpdate :: Bird -> Bird
birdUpdate b@Bird{..} = 
    if _dead then (updateAngle . birdFalling) b
             else (updateAngle . updatePicIndex . updateCount . birdFalling) b


updateCount :: Bird -> Bird
updateCount b@Bird{..} = 
    b { _count = if (fromIntegral _count) >= (__fFps / __fPps) then 0 else _count + 1 }


updatePicIndex :: Bird -> Bird
updatePicIndex b@Bird{..} = 
    let i = if _count == 0 
               then if _pIndex == (__nBirdAssets - 1) 
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

