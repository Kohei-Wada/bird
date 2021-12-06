module Options where

import Graphics.Gloss.Interface.IO.Game


__iFps :: Int
__iFps = 50


__fFps :: Float
__fFps = fromIntegral __iFps

__fPps :: Float
__fPps = 10 -- Picture per sec

__birdX :: Float 
__birdX = (-1) * __wWidth / 3  


__gravity :: Float
__gravity = 1000


__birdFlappingV :: Float
__birdFlappingV = -500.0 


__wWidth, __wHeight :: Num a => a
__wWidth  = 1000
__wHeight = 1000


__windowTitle :: String
__windowTitle = "Flappy Bird"


__birdAssets :: [FilePath] 
__birdAssets = 
    [ "/home/kohei/programs/bird/assets/bird-01.png"
    , "/home/kohei/programs/bird/assets/bird-02.png"
    , "/home/kohei/programs/bird/assets/bird-03.png"
    , "/home/kohei/programs/bird/assets/bird-04.png"
    ]
