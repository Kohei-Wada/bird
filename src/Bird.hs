{-#LANGUAGE RecordWildCards #-}

module Bird where
    
import Options

import Data.Maybe    
import Graphics.Gloss.Juicy
import Graphics.Gloss


data Bird = Bird 
    { _birdX        :: Float
    , _birdY        :: Float 
    , _velocity     :: Float
    , _acceleration :: Float
    , _birdPic      :: Picture
    } deriving Show


birdInit :: Bird
birdInit = Bird { _birdX        = (-1) * __wWidth / 3
                , _birdY        = __wHeight / 2 
                , _velocity     = 0 
                , _acceleration = __gravity 
                , _birdPic      = text $ show 0  
                }


updateBirdY :: Bird -> Bird
updateBirdY b@Bird{..} = 
    b { _birdY = _birdY - _velocity * (1.0 / __fFps) } 


updateBirdV :: Bird -> Bird
updateBirdV b@Bird{..} = 
    b { _velocity = _velocity + _acceleration * (1.0 / __fFps) }


updateBirdPic :: Bird -> Bird
updateBirdPic b@Bird{..} = b { _birdPic = text $ show _velocity }


birdFalling :: Bird -> Bird   
birdFalling b@Bird{..} = (updateBirdY . updateBirdV ) b


birdFlapping :: Bird -> Bird
birdFlapping b = b { _velocity = __birdFlappingV }


birdUpdate :: Bird -> Bird
birdUpdate b = (updateBirdPic . birdFalling) b










