{-#LANGUAGE RecordWildCards #-}
module Game where

import Bird
import Pipe
import Ground
import Sky
import Score
import Options
import GamePictures

import System.Exit

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game


data GameState = GameStart | GameStop | GameLoop | GameOver

data Game = Game 
    { _state    :: GameState
    , _bird     :: Bird
    , _sky      :: Sky
    , _ground   :: Ground
    , _pipes    :: [Pipe]
    , _score    :: Score
    , _pictures :: GamePictures 
    }


gameInit :: IO Game
gameInit = do 
    ps <- pipesInit __nPipes  
    gp <- loadAllPictures 

    return Game 
        { _state    = GameStart 
        , _bird     = birdInit 
        , _sky      = skyInit 
        , _ground   = groundInit  
        , _pipes    = ps
        , _score    = scoreInit 
        , _pictures = gp
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
      GameStart -> 
          return g 
              { _sky    = skyUpdate _sky
              , _ground = groundUpdate _ground 
              }

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
                 return g 
                     { _bird   = (if checkCollision g || checkCoordinates _bird
                                    then (`setBirdDead` True) else id) $birdUpdate _bird
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
             then let b@Bird{..} = _bird 
                      s = if groundCollision _ground _birdX _birdY 
                             then GameOver else _state 
                   in g { _state = s }
             else g 
      _ -> g


updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = 
    case _state of
      GameStart -> 
          updateGameObjects g

      GameStop -> 
          updateGameObjects g
         
      GameLoop -> 
          (updateGameObjects . updateGameState) g >>= return 

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

    GameStart -> 
        return $ pictures  
            [ skyPicture _pictures _sky
            , groundPicture _pictures _ground 
            , logoPicture _pictures 
            ]

    GameStop -> 
        return $ pictures  
            [ skyPicture _pictures _sky
            , groundPicture _pictures _ground 
            , birdPicture _pictures _bird  
            ]

    GameLoop -> 
        return $ pictures  
            [ skyPicture _pictures _sky
            , pictures $ pipesPicture _pictures _pipes
            , birdPicture _pictures _bird
            , groundPicture _pictures _ground
            , scorePicture _pictures _score
            ]

    GameOver -> 
        return $ pictures  
            [ skyPicture _pictures _sky
            , pictures $ pipesPicture _pictures _pipes
            , birdPicture _pictures _bird 
            , groundPicture _pictures _ground
            , scorePicture _pictures _score
            ]


eventHandler :: Event -> Game -> IO Game
eventHandler e g@Game{..} = case _state of 

    GameStart -> 
        case e of 
          EventKey (SpecialKey KeySpace) Down _ _ -> 
              return g { _state = GameStop }

          EventKey (Char 'k') Down _ _ -> 
              return g { _state = GameStop }

          EventKey (Char 'q') Down _ _ -> 
              exitSuccess

          _ -> 
              return g

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


