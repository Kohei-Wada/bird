{-#LANGUAGE RecordWildCards #-}
module Game where

import Bird
import Pipe
import Ground
import Sky
import Score
import Options
import Utils

import System.Exit

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


data GameState = GameStop | GameLoop | GameOver

data Game = Game 
    { _state  :: GameState
    , _bird   :: Bird
    , _sky    :: Sky
    , _ground :: Ground
    , _pipes  :: [Pipe]
    , _score  :: Score
    }


gameInit :: IO Game
gameInit = do 
    b  <- birdInit 
    s  <- skyInit
    g  <- groundInit
    ps <- pipesInit 2
    sc <- scoreInit 

    return Game 
        { _state  = GameStop
        , _bird   = b
        , _sky    = s
        , _ground = g
        , _pipes  = ps
        , _score  = sc
        }


gameRestart :: Game -> IO Game 
gameRestart g@Game{..} = do 
    ps <- resetPipes _pipes 
    return g
        { _state = GameLoop 
        , _bird  = birdReset _bird
        , _pipes = ps
        , _score = scoreReset _score 
        }


gameReset :: Game -> IO Game 
gameReset g@Game{..} = do 
    ps <- resetPipes _pipes 
    return g
        { _state = GameStop 
        , _bird  = birdReset _bird
        , _pipes = ps
        , _score = scoreReset _score 
        }


updateGameObjects :: Game -> IO Game
updateGameObjects g@Game{..} = 
    case _state of 
      GameStop -> 
          return g 
              { _sky    = skyUpdate _sky
              , _ground = groundUpdate _ground 
              }

      GameLoop -> do 
          if _dead _bird 
             then do 
                 return g 
                     { _bird   = birdUpdate _bird 
                     }
             else do 
                 ps <- pipesUpdate _pipes
                 let b = birdUpdate _bird
                 return g 
                     { _bird   = if checkCollision g || checkCoordinates b 
                                    then setBirdDead b True else b 
                     , _sky    = skyUpdate _sky 
                     , _ground = groundUpdate _ground
                     , _pipes  = ps
                     , _score  = updateScore _score _pipes _bird 
                     }

      GameOver ->
          return g


-- TODO 
updateGameState :: Game -> Game
updateGameState g@Game{..} = 
    case _state of 
      GameLoop -> 
          if _dead _bird 
             then 
                 let b@Bird{..} = _bird 
                     s = if groundCollision _ground _birdX _birdY 
                            then GameOver else _state 
                  in g { _state = s }
             else g 
      _ -> g


updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = 
    case _state of
      GameStop -> 
          updateGameObjects g
         
      GameLoop -> do 
          let g' = updateGameState g
          updateGameObjects g' >>= return 

      GameOver -> 
          updateGameObjects g


checkCollision :: Game -> Bool
checkCollision g@Game{..} = 
    let b@Bird{..} = _bird 
     in groundCollision _ground _birdX _birdY || pipesCollision _pipes _birdX _birdY 


checkCoordinates :: Bird -> Bool 
checkCoordinates b@Bird{..} = -_birdY > __wHeight || _birdY > __wHeight 


gameDisplay :: Game -> IO Picture
gameDisplay g@Game{..} = case _state of 
    GameStop -> 
        return $ pictures  
            [ skyPicture _sky
            , groundPicture _ground 
            , birdPicture _bird  
            ]

    GameLoop -> 
        return $ pictures  
            [ skyPicture _sky
            , pictures $ pipesPicture _pipes
            , groundPicture _ground
            , birdPicture _bird
            , scorePicture _score
            ]

    GameOver -> 
        return $ pictures  
            [ skyPicture _sky
            , pictures $ pipesPicture _pipes
            , groundPicture _ground
            , birdPicture _bird 
            , scorePicture _score
            ]


eventHandler :: Event -> Game -> IO Game
eventHandler e g@Game{..} = case _state of 
    GameStop -> 
        case e of 
          EventKey (SpecialKey KeySpace) Down _ _ -> 
              return g { _state = GameLoop }

          EventKey (Char 'k') Down _ _ -> 
              return g { _state = GameLoop }

          EventKey (Char 'q') Down _ _ -> 
              exitSuccess

          _ -> 
              return g

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

          EventKey (Char 'r') Down _ _ -> 
              gameRestart g

          EventKey (Char 'q') Down _ _ -> 
              exitSuccess
              
          _ ->
              return g
    
    GameOver -> 
        case e of 
          EventKey (SpecialKey KeySpace) Down _ _ -> do
              gameReset g

          EventKey (Char 'k') Down _ _ -> 
              gameReset g
 
          EventKey (Char 'q') Down _ _ -> 
              exitSuccess
 
          _ -> 
              return g


gameMain :: IO ()
gameMain = do
    let window = InWindow __winTitle (__wWidth, __wHeight) (500, 200)
    g <- gameInit
    playIO window __bkColor __iFps g gameDisplay eventHandler updateGame


