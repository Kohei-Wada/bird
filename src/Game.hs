{-#LANGUAGE RecordWildCards #-}
module Game where

import Bird
import Pipe
import Ground
import Sky
import Score
import Options
import GamePictures
import Actor

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
    , _hScore   :: Int
    }


gameInit :: IO Game
gameInit = do 
    ps <- pipesInit
    gp <- loadAllPictures 
    hs <- loadhighScore 
    b  <- initialize :: IO Bird
    s  <- initialize :: IO Sky
    g  <- initialize :: IO Ground

    pure Game 
        { _state    = GameStart 
        , _bird     = b
        , _sky      = s
        , _ground   = g
        , _pipes    = ps
        , _score    = scoreInit 
        , _pictures = gp
        , _hScore   = hs 
        }


gameRestart :: Game -> IO Game 
gameRestart g@Game{..} = do 
    ps <- resetPipes _pipes 
    pure g
        { _state = GameLoop 
        , _bird  = birdReset _bird
        , _pipes = ps
        , _score = scoreReset _score 
        }


gameReset :: Game -> IO Game 
gameReset g@Game{..} = do 
    ps <- resetPipes _pipes 
    if _value _score > _hScore 
       then do 
       writeHighScore $ _value _score
       pure g
           { _state = GameStop 
           , _bird  = birdReset _bird
           , _pipes = ps
           , _score = scoreReset _score 
           , _hScore = _value _score
           }

       else do  
       pure g
           { _state = GameStop 
           , _bird  = birdReset _bird
           , _pipes = ps
           , _score = scoreReset _score 
           }


updateGameObjects :: Game -> IO Game
updateGameObjects g@Game{..} = 
    case _state of 
      GameStart -> do 
          s' <- update _sky
          g' <- update _ground
          pure g 
              { _sky    = s'
              , _ground = g'
              }

      GameStop -> do 
          s' <- update _sky
          g' <- update _ground
          pure g 
              { _sky    = s'
              , _ground = g'
              }

      GameLoop -> do 
          if _dead _bird 
             then do 
                 b' <- update _bird
                 pure g { _bird = b' }

             else do 
                 ps <- mapM pipeUpdate _pipes
                 s' <- update _sky
                 g' <- update _ground
                 b' <- if checkCollision g || checkCoordinates _bird 
                          then pure $ (`setBirdDead` True) _bird
                          else update _bird 

                 pure g 
                     { _bird   = b'
                     , _sky    = s'
                     , _ground = g'
                     , _pipes  = ps
                     , _score  = updateScore _score _pipes _bird 
                     }

      GameOver -> pure g


updateGameState :: Game -> Game
updateGameState g@Game{..} = 
    case _state of 
      GameLoop -> 
          if _dead _bird 
             then g { _state = if groundCollision' _ground _bird then GameOver else _state }
             else g 
      _ -> g


updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = 
    case _state of
      GameStart -> updateGameObjects g
      GameStop  -> updateGameObjects g
      GameLoop  -> updateGameObjects $ updateGameState g 
      GameOver  -> updateGameObjects g


checkCollision :: Game -> Bool
checkCollision g@Game{..} = groundCollision' _ground _bird || any (\p -> pipeCollision p _bird) _pipes


checkCoordinates :: Bird -> Bool 
checkCoordinates b@Bird{..} = -_birdY > __wHeight || _birdY > __wHeight 


gameDisplay :: Game -> IO Picture
gameDisplay g@Game{..} = case _state of 
    GameStart -> 
        pure $ pictures  
            [ skyPicture _pictures _sky
            , groundPicture _pictures _ground 
            , logoPicture _pictures 
            ]

    GameStop -> 
        pure $ pictures  
            [ skyPicture _pictures _sky
            , groundPicture _pictures _ground 
            , birdPicture _pictures _bird  
            ]

    GameLoop -> 
        pure $ pictures  
            [ skyPicture _pictures _sky
            , pictures $ pipesPicture _pictures _pipes
            , birdPicture _pictures _bird
            , groundPicture _pictures _ground
            , scorePicture _pictures _score
            , highScorePicture _pictures _hScore
            ]

    GameOver -> 
        pure $ pictures  
            [ skyPicture _pictures _sky
            , pictures $ pipesPicture _pictures _pipes
            , birdPicture _pictures _bird 
            , groundPicture _pictures _ground
            , scorePicture _pictures _score
            , highScorePicture _pictures _hScore
--          , gameOverPicture _pictures 
            ]


eventHandler :: Event -> Game -> IO Game
eventHandler e g@Game{..} = case _state of 
    GameStart -> 
        case e of 
          EventKey (MouseButton LeftButton) Down _ _ -> 
              pure g { _state = GameStop }

          EventKey (SpecialKey KeySpace) Down _ _ -> 
              pure g { _state = GameStop }

          EventKey (Char 'k') Down _ _ -> 
              pure g { _state = GameStop }

          EventKey (Char 'q') Down _ _ -> 
              exitSuccess

          _ -> 
              pure g

    GameStop -> 
        case e of 
          EventKey (MouseButton LeftButton) Down _ _ -> 
              pure g { _state = GameLoop }

          EventKey (SpecialKey KeySpace) Down _ _ -> 
              pure g { _state = GameLoop }

          EventKey (Char 'k') Down _ _ -> 
              pure g { _state = GameLoop }

          EventKey (Char 'q') Down _ _ -> 
              exitSuccess

          _ -> 
              pure g

    GameLoop -> 
        case e of 
          EventKey (MouseButton LeftButton) Down _ _ -> 
              pure g { _bird = birdFlapping _bird } 

          EventKey (SpecialKey KeySpace) Down _ _ -> 
              pure g { _bird = birdFlapping _bird } 
                  
          EventKey (Char 'k') Down _ _ -> 
              pure g { _bird = birdFlapping _bird } 

          EventKey (Char 'j') Down _ _ -> 
              pure g { _bird = birdSwooping _bird }

          EventKey (Char 'l') Down _ _ -> 
              pure g 

          EventKey (Char 'h') Down _ _ -> 
              pure g 

          EventKey (Char 'r') Down _ _ -> 
              gameRestart g

          EventKey (Char 'q') Down _ _ -> 
              exitSuccess
              
          _ ->
              pure g
    
    GameOver -> 
        case e of 
          EventKey (MouseButton LeftButton) Down _ _ -> 
              gameReset g

          EventKey (SpecialKey KeySpace) Down _ _ -> 
              gameReset g
          EventKey (Char 'k') Down _ _ -> 
              gameReset g
 
          EventKey (Char 'q') Down _ _ -> 
              exitSuccess
 
          _ -> 
              pure g

gameMain :: IO ()
gameMain = do
    let window = InWindow __winTitle (__wWidth, __wHeight) (500, 200)
    g <- gameInit
    playIO window __bkColor __iFps g gameDisplay eventHandler updateGame
