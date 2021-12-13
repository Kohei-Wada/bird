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


data GameState = GameStop | GameLoop | GameOver

data Game = Game 
    { _state  :: GameState
    , _bird   :: Bird
    , _sky    :: Sky
    , _ground :: Ground
    , _pipes  :: [Pipe]
    , _score  :: Int
    }


gameInit :: IO Game
gameInit = do 
    b  <- birdInit 
    s  <- skyInit
    g  <- groundInit
    ps <- pipesInit 2

    return Game 
        { _state  = GameStop
        , _bird   = b
        , _sky    = s
        , _ground = g
        , _pipes  = ps
        , _score  = 0
        }


gameRestart :: Game -> IO Game 
gameRestart g@Game{..} = do 
    ps <- resetPipes _pipes 
    return g
        { _state = GameLoop 
        , _bird  = birdReset _bird
        , _pipes = ps
        }


gameReset :: Game -> IO Game 
gameReset g@Game{..} = do 
    ps <- resetPipes _pipes 
    return g
        { _state = GameStop 
        , _bird  = birdReset _bird
        , _pipes = ps
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
          ps <- pipesUpdate _pipes
          return g 
              { _bird   = birdUpdate _bird
              , _sky    = skyUpdate _sky 
              , _ground = groundUpdate _ground
              , _pipes  = ps
              }

      GameOver ->
          return g

-- TODO 
updateGameState :: Game -> Game
updateGameState g@Game{..} = 
    let s = if checkCoordinates _bird || checkCollision g then GameOver else _state
     in g { _state = s }


updateGame :: Float -> Game -> IO Game
updateGame _ g@Game{..} = 
    case _state of
      GameStop -> 
          updateGameObjects g
         
      GameLoop -> do 
          g' <- updateGameObjects g
          return $ updateGameState g'

      GameOver -> 
          updateGameObjects g


checkCollision :: Game -> Bool
checkCollision g@Game{..} = 
    let b@Bird{..} = _bird 
     in groundCollision _ground _birdX _birdY || pipesCollision _pipes _birdX _birdY 


checkCoordinates :: Bird -> Bool 
checkCoordinates b@Bird{..} = -_birdY > __wHeight || _birdY > __wHeight 


pipesPicture :: [Pipe] -> [Picture]
pipesPicture =  map pipePicture 


pipePicture :: Pipe -> Picture
pipePicture p@Pipe{..} = 
    let tmpUp = makeLongPicH _pipePic __wHeight __pipeHgt 
        tmpDw = makeLongPicH _pipePic __wHeight (- __pipeHgt)
     in pictures [ translate _pipeX _pipeUp tmpUp
                 , translate _pipeX _pipeDw tmpDw
                 , translate _pipeX _pipeUp _pipePicUp 
                 , translate _pipeX _pipeDw _pipePicDw
                 ] 


birdPicture :: Bird -> Picture
birdPicture b@Bird{..} = 
   translate _birdX _birdY $ rotate _angle (_birdPics !! _pIndex) 


groundPicture :: Ground -> Picture
groundPicture g@Ground{..} = translate _groundX _groundY _groundPic 


skyPicture :: Sky -> Picture
skyPicture s@Sky{..} = translate _skyX  _skyY  _skyPic 


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
            ]

    GameOver -> 
        return $ pictures  
            [ skyPicture _sky
            , pictures $ pipesPicture _pipes
            , groundPicture _ground
            , birdPicture _bird 
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


