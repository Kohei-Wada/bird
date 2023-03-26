{-# LANGUAGE BangPatterns #-}
module Bench where

import System.CPUTime

import Bird
import Ground
import Sky
import Pipe
import Score
import Game
import Actor

import Control.Monad


bench :: IO a -> IO () 
bench m = do 
    results <- forM [1..10] $ \i -> do 
        start <- getCPUTime
        m
        end <- getCPUTime
        pure (end - start)

    print results
    print $ "it takes " <> show (average results) <> "[ps]"

    where
        average :: [Integer] -> Integer
        average ns = sum ns `div` fromIntegral (length ns)

benchMain :: IO () 
benchMain = do 
    benchUpdateBird
    benchUpdateSky
    benchUpdateGround
    benchUpdatePipes
    benchUpdateGameState
    benchUpdateGameObjects
    benchResetGame
    benchUpdateGame

benchUpdateSky :: IO () 
benchUpdateSky = do 
    print "sky"
    !s <- initialize :: IO Sky
    bench $ pure $ update s

benchUpdateBird :: IO ()
benchUpdateBird = do 
    print "bird"
    !b <- initialize :: IO Bird
    bench $ pure $ update b

benchUpdateGround :: IO ()
benchUpdateGround = do 
    print "ground"
    !g <- initialize :: IO Ground
    bench $ pure $ update g

benchUpdatePipes :: IO () 
benchUpdatePipes = do 
    print "pipes"
    !p <- initialize :: IO Pipes
    bench $ pure $ update p

benchUpdateGameState :: IO () 
benchUpdateGameState = do 
    print "game state"
    !g <- gameInit
    bench $ pure $ updateGameState g

benchUpdateGameObjects :: IO () 
benchUpdateGameObjects = do 
    print "game objects"
    !g <- gameInit
    bench $ pure $ updateGameObjects g

benchUpdateGame :: IO () 
benchUpdateGame = do 
    print "game"
    !g <- gameInit
    bench $ updateGame 0 g

benchResetGame :: IO () 
benchResetGame = do 
    print "game reset"
    !g <- gameInit
    bench $ gameReset g


