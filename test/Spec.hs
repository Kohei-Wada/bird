import System.IO.Silently
import Control.Monad
import Test.HUnit

import BirdTest (birdTests)

testHello :: Test
testHello = TestCase $ do
    let expectedOutput = concat $ replicate 20 "Hello\n"
    actualOutput <- capture_ $ putStrLn "Hello" >> replicateM_ 19 (putStrLn "Hello")
    assertEqual "Output does not match" expectedOutput actualOutput


test2 :: Test
test2 = TestCase $ do 
    print "hello"

tests :: Test
tests = TestList
    [ TestLabel "testHello" testHello
    , birdTests
    ]
    
main :: IO Counts
main = runTestTT tests
