module Options where

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


{- Game -}
__iFps :: Int
__iFps = 50

__fFps :: Float
__fFps = fromIntegral __iFps

__defaultWinWidth, __defaultWinHeight :: Num a => a
__defaultWinWidth  = 276 
__defaultWinHeight = 600

__wWidth, __wHeight :: Num a => a
__wWidth  = __defaultWinWidth * 3 
__wHeight = __defaultWinHeight

__winTitle :: String
__winTitle = "Flappy Bird"

__bkColor :: Color 
__bkColor = light $ light $ light blue


{- Bird -}
__birdX, __birdY :: Float 
__birdX = (-1) * __wWidth / 8 
__birdY = 0.0

__gravity :: Float
__gravity = 1000

__birdFlappingV :: Float
__birdFlappingV = -500.0 

__maxBirdAngle, __minBirdAngle :: Float
__maxBirdAngle = 90
__minBirdAngle = -45

__angleBias :: Float
__angleBias = 10

__fPps :: Float
__fPps = 10 -- Picture per sec

__birdAssets :: [FilePath] 
__birdAssets = 
    [ "/home/kohei/programs/bird/assets/bird-01.png"
    , "/home/kohei/programs/bird/assets/bird-02.png"
    , "/home/kohei/programs/bird/assets/bird-03.png"
    , "/home/kohei/programs/bird/assets/bird-04.png"
    ]


{- Pipe -}

__defaultPipeWid :: Int
__defaultPipeWid = 52 

__pipeHgt :: Int
__pipeHgt = 1

__pipeSpeed :: Float
__pipeSpeed = -200.0

__pipeAssets :: [FilePath]
__pipeAssets = 
    [ "/home/kohei/programs/bird/assets/pipe-down.png"
    , "/home/kohei/programs/bird/assets/pipe-up.png"
    , "/home/kohei/programs/bird/assets/pipe.png"
    ]


{- Sky -}
__defaultSkyWid :: Int
__defaultSkyWid = 276

__skyWid :: Int
__skyWid = __defaultSkyWid 

__defaultSkyX :: Float
__defaultSkyX = 0.0

__defaultSkyY :: Float 
__defaultSkyY =  -150.0

__skySpeed :: Float
__skySpeed =  -20

__skyAssets :: [FilePath]
__skyAssets = 
    [ "/home/kohei/programs/bird/assets/sky.png" 
    ]


{- Ground -}
__defaultGroundX, __defaultGroundY :: Float
__defaultGroundX = 0.0
__defaultGroundY = -250.0

__groundSpeed :: Float
__groundSpeed = __pipeSpeed  

__defaultGroundWid :: Int
__defaultGroundWid = 336

__groundWid :: Int
__groundWid = __defaultGroundWid 

__groundAssets :: [FilePath]
__groundAssets = 
    [ "/home/kohei/programs/bird/assets/land.png" 
    ]



