module Options where

import Graphics.Gloss


{- Game -}
__iFps     = 30                  :: Int
__fFps     = fromIntegral __iFps :: Float
__winTitle = "Flappy Bird"
__bkColor  = light $ light $ light blue

__winWidth__ , __winHeight__ , __wWidth, __wHeight :: Num a => a
__winWidth__  = 276  -- WARNING : Don't change
__winHeight__ = 600  -- WARNING : Don't change
__wWidth      = __winWidth__ * 2
__wHeight     = __winHeight__


{- Bird -}
__birdX         = (-1) * __wWidth / 8 :: Float
__birdY         = 0.0                 :: Float 
__gravity       = 2000                :: Float
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
__PipeWid__      = 52     :: Int  -- Warning : Original picture size , Don't change.
__pipesGap       = -120   :: Float
__pipeHgt        = 1      :: Int
__pipeSpeed      = -150.0 :: Float
__pipeAssets     = 
    [ "/home/kohei/programs/bird/assets/pipe-down.png"
    , "/home/kohei/programs/bird/assets/pipe-up.png"
    , "/home/kohei/programs/bird/assets/pipe.png"
    ]


{- Sky -}
__skyWid__      = 276             :: Int -- Warning : Original picture size, Don't change.
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
__groundWid__    = 336           :: Int -- Warning : Original picture size, Don't change.
__groundWid      = __groundWid__ :: Int
__groundAssets   = 
    [ "/home/kohei/programs/bird/assets/land.png" 
    ]



