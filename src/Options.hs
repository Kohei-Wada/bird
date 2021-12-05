module Options where

import Graphics.Gloss.Interface.IO.Game


stageColor :: Color
stageColor = light $ light $ light $ green


wWidth, wHeight :: Num a => a
wWidth  = 1000
wHeight = 1000


windowTitle :: String
windowTitle = "Flappy Bird"



birdAssets :: [String] 
birdAssets = 
    [ "bird-01.png"
    , "bird-02.png"
    , "bird-03.png"
    , "bird-04.png"
    ]
