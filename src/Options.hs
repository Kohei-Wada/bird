module Options where

import Graphics.Gloss


{- Game -}
__iFps     = 50                  :: Int
__fFps     = fromIntegral __iFps :: Float
__winTitle = "Flappy Bird"
__bkColor  = light $ light $ light blue

__winWidth__ , __winHeight__ , __wWidth, __wHeight :: Num a => a
__winWidth__  = 276  -- WARNING : Don't change
__winHeight__ = 600  -- WARNING : Don't change
__wWidth      = __winWidth__ * 3
__wHeight     = __winHeight__


{- Bird -}
__birdWid__     = 34                  :: Int
__birdHgt__     = 24                  :: Int
__birdX         = (-1) * __wWidth / 5 :: Float
__birdY         = __winHeight__ / 8   :: Float 
__gravity       = 2000                :: Float
__birdFlappingV = -500.0              :: Float
__maxBirdAngle  = 90                  :: Float
__minBirdAngle  = -45                 :: Float
__angleBias     = 10                  :: Float
__fPps          = 30                  :: Float -- Picture per sec
__birdAssets    = 
    [ "assets/bird-01.png"
    , "assets/bird-02.png"
    , "assets/bird-03.png"
    , "assets/bird-04.png"
    ]


{- Pipe -}
__pipeWid__      = 52     :: Int  -- Warning : Original picture size , Don't change.
__pipesGap       = -120   :: Float
__pipeHgt        = 1      :: Int
__pipeSpeed      = -150.0 :: Float
__pipeAssets     = 
    [ "assets/pipe-down.png"
    , "assets/pipe-up.png"
    , "assets/pipe.png"
    ]


{- Sky -}
__skyWid__      = 276             :: Int -- Warning : Original picture size, Don't change.
__defaultSkyX   = 0.0             :: Float
__defaultSkyY   = -150.0          :: Float
__skySpeed      = -50             :: Float
__skyWid        = __skyWid__      :: Int
__skyAssets     = 
    [ "assets/sky.png" 
    ]


{- Ground -}
__defaultGroundX      = 0.0           :: Float
__defaultGroundY      = -250.0        :: Float
__groundSpeed         = __pipeSpeed   :: Float
__groundWid__         = 336           :: Int -- Warning : Original picture size, Don't change.
__groundWid           = __groundWid__ :: Int
__groundCollisionBias = 80            :: Float
__groundAssets        = 
    [ "assets/land.png" 
    ]


{- Score -}
__scoreWid__ = 20 :: Float
__scoreAssets = 
    [ "assets/font_big_0.png"
    , "assets/font_big_1.png"
    , "assets/font_big_2.png"
    , "assets/font_big_3.png"
    , "assets/font_big_4.png"
    , "assets/font_big_5.png"
    , "assets/font_big_6.png"
    , "assets/font_big_7.png"
    , "assets/font_big_8.png"
    , "assets/font_big_9.png"
    ]

