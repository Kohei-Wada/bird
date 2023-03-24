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
    print "this is a bench mark"
    benchUpdateBird
    benchUpdateSky
    benchUpdateGround
    benchUpdatePipes

benchUpdateSky :: IO () 
benchUpdateSky = do 
    s <- initialize :: IO Sky
    bench $ pure $ update s

benchUpdateBird :: IO ()
benchUpdateBird = do 
    b <- initialize :: IO Bird
    bench $ pure $ update b

benchUpdateGround :: IO ()
benchUpdateGround = do 
    g <- initialize :: IO Ground
    bench $ pure $ update g

benchUpdatePipes :: IO () 
benchUpdatePipes = do 
    p <- initialize :: IO Pipes
    bench $ pure $ update p

