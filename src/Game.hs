{-#LANGUAGE RecordWildCards #-}

module Game where

import Bird
import Pipe
import Ground
import Sky
import Options
import Utils
import System.Exit

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy


data GameState = GameStop | GameLoop | GameOver

data Game = Game 
    { _state  :: !GameState
    , _bird   :: !Bird
    , _sky    :: !Sky
    , _ground :: !Ground
    , _pipes  :: ![Pipe]
    , _score  :: !Int
    }


gameInit :: IO Game
gameInit = do 
    b  <- birdInit 
    s  <- skyInit
    g  <- groundInit
    ps <- pipesInit 1
    return Game { _state  = GameLoop
                , _bird   = b
                , _sky    = s
                , _ground = g
                , _pipes  = ps
                , _score  = 0
                }


pipePicture :: Pipe -> Picture
pipePicture p@Pipe{..} = 
    let tmpUp = makeLongPicH _pipePic __wHeight __pipeHgt
        tmpDw = makeLongPicH _pipePic __wHeight (- __pipeHgt)
     in pictures [ translate _pipeX _pipeUp tmpUp
                 , translate _pipeX _pipeDw tmpDw
                 , translate _pipeX _pipeUp _pipePicUp 
                 , translate _pipeX _pipeDw _pipePicDw
                 ] 


pipesPicture :: [Pipe] -> [Picture]
pipesPicture ps =  map pipePicture ps


gameDisplay :: Game -> IO Picture
gameDisplay g@Game{..} = case _state of 
    GameStop -> return blank
    GameLoop -> display
    GameOver -> display

    where display :: IO Picture
          display = do 
            let g@Ground{..} = _ground
                s@Sky{..}   = _sky

            return $ pictures  
                [ translate _skyX  _skyY  _skyPic 
                , pictures $ pipesPicture _pipes
                , translate _groundX _groundY _groundPic 
                , _birdPic _bird
                ]



eventHandler :: Event -> Game -> IO Game
eventHandler e g@Game{..} = case _state of 
    GameStop -> return g
    GameLoop -> 
        case e of 
          EventKey (SpecialKey KeySpace) Down _ _ -> 
              return g { _bird = birdFlapping _bird } 
                  
          EventKey (Char 'k') Down _ _ -> 
              return g { _bird = birdFlapping _bird } 

          EventKey (Char 'l') Down _ _ -> 
              return g 

          EventKey (Char 'h') Down _ _ -> 
              return g 

          EventKey (Char 'q') Down _ _ -> 
              exitSuccess
              
          _ ->
              return g
    
    GameOver -> return g


--TODO
updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = case _state of
    GameStop -> return g

    GameLoop ->  do 
        ps <- pipesUpdate _pipes
        let b = birdUpdate _bird
            s = if b == BirdDead then GameOver else _state

         in return 
               g { _state  = s
                 , _bird   = if b == BirdDead then birdReset _bird else b
                 , _sky    = skyUpdate _sky 
                 , _ground = groundUpdate _ground
                 , _pipes  = ps
                 }

    GameOver -> return g { _state = GameLoop }


gameMain :: IO ()
gameMain = do
    let window = InWindow __winTitle (__wWidth, __wHeight) (500, 200)
    g <- gameInit
    playIO window __bkColor __iFps g gameDisplay eventHandler updateGame


