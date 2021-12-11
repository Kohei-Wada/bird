module Options where

import Graphics.Gloss


{- Game -}
__iFps     = 50                  :: Int
__fFps     = fromIntegral __iFps :: Float
__winTitle = "Flappy Bird"
__bkColor  = light $ light $ light blue

__defaultWinWidth, __defaultWinHeight , __wWidth, __wHeight :: Num a => a
__defaultWinWidth  = 276 
__defaultWinHeight = 600
__wWidth           = __defaultWinWidth * 2 
__wHeight          = __defaultWinHeight


{- Bird -}
__birdX         = (-1) * __wWidth / 8 :: Float
__birdY         = 0.0                 :: Float 
__gravity       = 1500                :: Float
__birdFlappingV = -500.0              :: Float
__maxBirdAngle  = 90                  :: Float
__minBirdAngle  = -45                 :: Float
__angleBias     = 10                  :: Float
__fPps          = 10                  :: Float -- Picture per sec
__birdAssets    = 
    [ "/home/kohei/programs/bird/assets/bird-01.png"
    , "/home/kohei/programs/bird/assets/bird-02.png"
    , "/home/kohei/programs/bird/assets/bird-03.png"
    , "/home/kohei/programs/bird/assets/bird-04.png"
    ]


{- Pipe -}
__PipeWid__      = 52     :: Int  -- Original picture size
__pipesGap       = -150   :: Float
__pipeHgt        = 1      :: Int
__pipeSpeed      = -200.0 :: Float
__pipeAssets     = 
    [ "/home/kohei/programs/bird/assets/pipe-down.png"
    , "/home/kohei/programs/bird/assets/pipe-up.png"
    , "/home/kohei/programs/bird/assets/pipe.png"
    ]


{- Sky -}
__skyWid__      = 276             :: Int -- Original picture size
__defaultSkyX   = 0.0             :: Float
__defaultSkyY   = -150.0          :: Float
__skySpeed      = -50             :: Float
__skyWid        = __skyWid__      :: Int
__skyAssets     = 
    [ "/home/kohei/programs/bird/assets/sky.png" 
    ]


{- Ground -}
__defaultGroundX = 0.0           :: Float
__defaultGroundY = -250.0        :: Float
__groundSpeed    = __pipeSpeed   :: Float
__groundWid__    = 336           :: Int -- Original picture size
__groundWid      = __groundWid__ :: Int
__groundAssets   = 
    [ "/home/kohei/programs/bird/assets/land.png" 
    ]



