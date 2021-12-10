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
    { _state  :: GameState
    , _bird   :: Bird
    , _sky    :: Sky
    , _ground :: Ground
    , _pipe   :: Pipe
    , _score  :: Int
    }


gameInit :: IO Game
gameInit = do 
    b <- birdInit 
    s <- skyInit
    g <- groundInit
    p <- pipeInit
    return Game { _state  = GameLoop
                , _bird   = b
                , _sky    = s
                , _ground = g
                , _pipe   = p
                , _score  = 0
                }


gameDisplay :: Game -> IO Picture
gameDisplay g@Game{..} = case _state of 
    GameStop -> return blank

    GameLoop -> do 
        let p@Pipe{..}  = _pipe
            g@Ground{..} = _ground
            s@Sky{..}   = _sky

            tmpUp = makeLongPicH _pipePic (round $ (fromIntegral __wHeight) / 2)__pipeHgt
            tmpDw = makeLongPicH _pipePic (round $ (fromIntegral __wHeight) / 2) (- __pipeHgt)

        return $ pictures  
            [ translate _skyX  _skyY  _skyPic 
            , translate _pipeX _pipeUp tmpUp
            , translate _pipeX _pipeDw tmpDw
            , translate _pipeX _pipeUp _pipePicUp 
            , translate _pipeX _pipeDw _pipePicDw
            , translate _groundX _groundY _groundPic 
            , _birdPic _bird
            ]

    GameOver -> return blank


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


updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = case _state  of
    GameStop -> return g
    GameLoop -> 
        let b = birdUpdate _bird
         in return g { _bird = if b == BirdDead then birdReset _bird else b
                     , _sky  = skyUpdate _sky 
                     , _ground = groundUpdate _ground
                     , _pipe = pipeUpdate _pipe
                     }


gameMain :: IO ()
gameMain = do
    let window = InWindow __winTitle (__wWidth, __wHeight) (500, 200)
    g <-  gameInit
    playIO window __bkColor __iFps g gameDisplay eventHandler updateGame



