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


gameInit :: IO Game
gameInit = do 
    b <- birdInit 
    return Game { _state = GameLoop
                , _bird  = b
                , _score = 0
                }


gameDisplay :: Game -> IO Picture
gameDisplay g@Game{..} = case _state of 
    GameStop -> return blank
    GameLoop -> return $ translate (_birdX _bird) (_birdY _bird) (_birdPic _bird)  
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
              return g { _bird = moveBirdX _bird 10} 

          EventKey (Char 'h') Down _ _ -> 
              return g { _bird = moveBirdX _bird (-10)}

          EventKey (Char 'q') Down _ _ -> 
              exitSuccess
              
          _ ->
              return g
    
    GameOver -> return g


updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = do 
    return g { _bird = birdUpdate _bird }


gameMain :: IO ()
gameMain = do
    let window = InWindow __windowTitle (__wWidth, __wHeight) (100, 100)
    g <-  gameInit

    playIO window white __iFps g gameDisplay eventHandler updateGame




