{-#LANGUAGE RecordWildCards #-}

module Bird where
    
import Options

import Data.Maybe    
import Graphics.Gloss.Juicy
import Graphics.Gloss


data Bird = Bird 
    { _height :: Double 
    , _vel    :: Double
    } deriving Show


birdInit :: Bird
birdInit = Bird { _height = 100 
                , _vel = 100
                }


birdFalling :: Bird -> Bird
birdFalling b@Bird{..} = b


birdFlapping :: Bird -> Bird
