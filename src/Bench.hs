module Bench where

import System.CPUTime

import Bird
import Ground
import Sky
import Pipe
import Score
import Game
import Actor


bench :: IO a -> IO () 
bench m = do 
    start <- getCPUTime
    m
    end <- getCPUTime
    print $ "it takes " <> show (end - start) <> "[ps]"


benchMain :: IO () 
benchMain = do 
    benchUpdateBird
    benchUpdateSky
    benchUpdateGround
    benchUpdatePipes
    benchUpdateGameState
    benchUpdateGameObjects
    benchUpdateGame

benchUpdateSky :: IO () 
benchUpdateSky = do 
    print "sky"
    s <- initialize :: IO Sky
    bench $ pure $ update s

benchUpdateBird :: IO ()
benchUpdateBird = do 
    print "bird"
    b <- initialize :: IO Bird
    bench $ pure $ update b

benchUpdateGround :: IO ()
benchUpdateGround = do 
    print "ground"
    g <- initialize :: IO Ground
    bench $ pure $ update g

benchUpdatePipes :: IO () 
benchUpdatePipes = do 
    print "pipes"
    p <- initialize :: IO Pipes
    bench $ pure $ update p

benchUpdateGameState :: IO () 
benchUpdateGameState = do 
    print "game state"
    g <- gameInit
    bench $ pure $ updateGameState g

benchUpdateGameObjects :: IO () 
benchUpdateGameObjects = do 
    print "game objects"
    g <- gameInit
    bench $ pure $ updateGameObjects g

benchUpdateGame :: IO () 
benchUpdateGame = do 
    print "game"
    g <- gameInit
    bench $ updateGame 0 g


