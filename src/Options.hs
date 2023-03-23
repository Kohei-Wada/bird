module Options where

import Graphics.Gloss


{- Game -}
__iFps     = 60                  :: Int
__dFps     = fromIntegral __iFps :: Double
__winTitle = "Flappy Bird"
__bkColor  = cyan

--__winWidth__ , __winHeight__ , __wWidth, __wHeight :: Num a => a
__winWidth__  = 276 :: Double  -- WARNING : Don't change 
__winHeight__ = 600 :: Double -- WARNING : Don't change
__wWidth      = __winWidth__ * 3.0 :: Double
__wHeight     = __winHeight__ :: Double


{- Bird -}
__birdWid__     = 34                  :: Int
__birdHgt__     = 24                  :: Int
__birdX         = (-1) * __wWidth / 5.0 :: Double
__birdY         = __winHeight__ / 8.0   :: Double
__gravity       = 2000                :: Double
__birdFlappingV = -500.0              :: Double
__birdSwoopingV = 500.0               :: Double
__maxBirdAngle  = 90                  :: Double
__minBirdAngle  = -45                 :: Double
__angleBias     = 10                  :: Double
__fPps          = 30                  :: Double -- Picture per sec
__nBirdAssets   = 4                   :: Int
__birdAssets    = 
    [ "assets/bird-01.png"
    , "assets/bird-02.png"
    , "assets/bird-03.png"
    , "assets/bird-04.png"
    ]


{- Pipe -}
__pipeWid__      = 52     :: Int  -- Warning : Original picture size , Don't change.
__pipesGap       = -120   :: Double
__pipeHgt        = 1      :: Int
__nPipes         = 3      :: Int
__pipeSpeed      = -150.0 :: Double
__pipeAssets     = 
    [ "assets/pipe-down.png"
    , "assets/pipe-up.png"
    , "assets/pipe.png"
    ]


{- Sky -}
__skyWid__      = 276             :: Int -- Warning : Original picture size, Don't change.
__defaultSkyX   = 0.0             :: Double
__defaultSkyY   = -150.0          :: Double
__skySpeed      = -50             :: Double
__skyWid        = __skyWid__      :: Int
__skyAssets     = 
    [ "assets/sky.png" 
    ]


{- Ground -}
__defaultGroundX      = 0.0           :: Double
__defaultGroundY      = -250.0        :: Double
__groundSpeed         = __pipeSpeed   :: Double
__groundWid__         = 336           :: Int -- Warning : Original picture size, Don't change.
__groundWid           = __groundWid__ :: Int
__groundCollisionBias = 80            :: Double
__groundResetBias     = 10            :: Double
__groundAssets        = 
    [ "assets/land.png" 
    ]


{- Score -}
__scoreWid__ = 20 :: Double
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

__scoreData__ = "data/.highScore.dat"


{- Logo -}
__logoAssets = 
    [ "assets/logo.png" ]

__gameOver = 
    [ "assets/gameover.png" ]
