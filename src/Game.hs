{-#LANGUAGE RecordWildCards #-}

module Game where

import Bird
import Options
import System.Exit

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Juicy


data GameState = GameStop | GameLoop | GameOver

data Game = Game 
    { _state :: GameState
    , _bird  :: Bird
    , _score :: Int
    }


gameInit :: Game
gameInit = Game 
    { _state = GameLoop
    , _bird  = birdInit
    , _score = 0
    }


gameDisplay :: Game -> IO Picture
gameDisplay g@Game{..} = case _state of 
    GameStop -> return blank
    GameLoop -> return blank
    GameOver -> return blank



eventHandler :: Event -> Game -> IO Game
eventHandler e g@Game{..} = case _state of 
    GameStop -> return g
    GameLoop -> case e of 
                  EventKey (SpecialKey KeySpace) Down _ _ -> 
                      return g { _score = _score + 1 }
                  EventKey (Char 'q') Down _ _ -> 
                      exitSuccess
                      
                  _ -> return g
    
    GameOver -> return g

updateGame :: Float -> Game -> IO Game
updateGame _ g = return g 



gameMain :: IO ()
gameMain = do
    let window = InWindow windowTitle (wWidth, wHeight) (100, 100)
        g      = gameInit

    playIO window white 20 g gameDisplay eventHandler updateGame




