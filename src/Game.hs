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

import Control.Monad
import Control.Monad.ST
import Data.STRef
import GHC.IO (ioToST)

data GameState = GameStart | GameStop | GameLoop | GameOver

data Game = Game 
    { _state    :: !GameState
    , _bird     :: !Bird
    , _sky      :: !Sky
    , _ground   :: !Ground
    , _pipes    :: !Pipes
    , _score    :: !Score
    , _pictures :: !GamePictures 
    , _hScore   :: !Int
    }

gameInit :: IO Game
gameInit = do 
    gp <- loadAllPictures 
    hs <- loadhighScore 
    ps <- initialize
    b  <- initialize 
    s  <- initialize
    g  <- initialize
    sc <- scoreInit
    pure Game 
        { _state    = GameStart 
        , _bird     = b
        , _sky      = s
        , _ground   = g
        , _pipes    = ps
        , _score    = sc 
        , _pictures = gp
        , _hScore   = hs 
        }


gameRestart :: Game -> IO Game 
gameRestart g@Game{..} = do 
    ps <- initialize
    b  <- initialize
    sc <- scoreInit
    pure g
        { _state = GameLoop 
        , _bird  = b
        , _pipes = ps
        , _score = sc
        }

gameReset :: Game -> IO Game 
gameReset g@Game{..} = do 
    ps <- initialize
    b  <- initialize
    sc <- scoreInit
    if _value _score > _hScore 
       then do 
       writeHighScore _score
       pure g
           { _state  = GameStop 
           , _bird   = b
           , _pipes  = ps
           , _score  = sc
           , _hScore = _value _score
           }

       else do  
       pure g
           { _state = GameStop 
           , _bird  = b
           , _pipes = ps
           , _score = sc
           }


updateGameObjects :: Game -> IO Game
updateGameObjects g@Game{..} = do 
    s' <- update _sky
    g' <- update _ground

    case _state of 
      GameStart -> do 
          pure g { _sky    = s'
                 , _ground = g'
                 }

      GameStop -> do 
          pure g { _sky    = s'
                 , _ground = g'
                 }

      GameLoop -> do 
          if _dead _bird 
             then do 
                 b' <- update _bird
                 pure g { _bird = b' }

             else do 
                 ps <- update (_pipes)
                 b' <- if checkCollision g || checkCoordinates _bird 
                          then pure $ birdKill _bird
                          else update _bird 
                 pure g 
                     { _bird   = b'
                     , _sky    = s'
                     , _ground = g'
                     , _pipes  = ps
                     , _score  = updateScore _score (_pipes) _bird 
                     }

      GameOver -> pure g


updateGameState :: Game -> Game
updateGameState g@Game{..} = runST $ do 
    g' <- newSTRef g
    modifySTRef g' updateGameState' 
    readSTRef g'
        where 
            updateGameState' g = case _state of 
              GameLoop -> 
                  if _dead _bird 
                     then g { _state = if groundCollision _ground _bird then GameOver else _state }
                     else g 
              _ -> g
            {-# INLINE updateGameState' #-}

updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = 
    case _state of
      GameStart -> updateGameObjects g
      GameStop  -> updateGameObjects g
      GameLoop  -> updateGameObjects $ updateGameState g 
      GameOver  -> updateGameObjects g


checkCollision :: Game -> Bool
checkCollision Game{..} = groundCollision _ground _bird || pipesCollision _pipes _bird


checkCoordinates :: Bird -> Bool 
checkCoordinates Bird{..} = -_birdY > __wHeight || _birdY > __wHeight 


gameDisplay :: Game -> IO Picture
gameDisplay Game{..} = case _state of 
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
            , pictures $ pipesPicture _pictures (_pipes)
            , birdPicture _pictures _bird
            , groundPicture _pictures _ground
            , scorePicture _pictures _score
            , highScorePicture _pictures _hScore
            ]

    GameOver -> 
        pure $ pictures  
            [ skyPicture _pictures _sky
            , pictures $ pipesPicture _pictures (_pipes)
            , birdPicture _pictures _bird 
            , groundPicture _pictures _ground
            , scorePicture _pictures _score
            , highScorePicture _pictures _hScore
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
    let window = InWindow __winTitle (round __wWidth, round __wHeight) (500, 200)
    g <- gameInit
    playIO window __bkColor __iFps g gameDisplay eventHandler updateGame
