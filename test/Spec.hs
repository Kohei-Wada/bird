import Test.HUnit
import Score
import Pipe
import Bird
import Options
import Game
import Graphics.Gloss.Interface.IO.Game (Event(..), Key(..), KeyState(..), SpecialKey(..), Modifiers(..))

-- Helper for approximate floating point comparison
assertApproxEqual :: String -> Double -> Double -> Double -> Assertion
assertApproxEqual msg expected actual epsilon = assertBool msg (abs (expected - actual) < epsilon)

-- Example test for addScore
testAddScore :: Test
testAddScore = TestCase $ do
    let initialScore = Score {_value = 0, _scoreX = 0, _scoreY = 0, _sFlag = False, _highScore = 0}
    let newScore = addScore initialScore
    assertEqual "Should increment score by 1" 1 (_value newScore)

-- Example test for updateScore
testUpdateScore :: Test
testUpdateScore = TestCase $ do
    let bird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let pipes = Pipes []
    let initialScore = Score {_value = 0, _scoreX = 0, _scoreY = 0, _sFlag = True, _highScore = 0}
    let newScore = updateScore initialScore pipes bird
    assertEqual "Score should be incremented when bird is not in a pipe gap" 1 (_value newScore)

-- Test for parseHighScore
testParseHighScore :: Test
testParseHighScore = TestCase $ do
    assertEqual "Should parse valid score string" 123 (parseHighScore "123")
    assertEqual "Should return 0 for invalid string" 0 (parseHighScore "invalid")
    assertEqual "Should return 0 for empty string" 0 (parseHighScore "")

-- Test for formatHighScore
testFormatHighScore :: Test
testFormatHighScore = TestCase $ do
    let score = Score {_value = 42, _scoreX = 0, _scoreY = 0, _sFlag = False, _highScore = 100}
    assertEqual "Should format score to string" "42" (formatHighScore score)

-- Test for gameInit
testGameInit :: Test
testGameInit = TestCase $ do
    game <- gameInit
    assertEqual "Initial game state should be GameStart" GameStart (_state game)

-- Test for eventHandler state transition
testEventHandler_GameStart_to_GameStop :: Test
testEventHandler_GameStart_to_GameStop = TestCase $ do
    initialGame <- gameInit
    let event = EventKey (SpecialKey KeySpace) Down (Modifiers { shift = Up, ctrl = Up, alt = Up }) (0,0)
    finalGame <- eventHandler event initialGame
    assertEqual "State should transition from GameStart to GameStop on Space key" GameStop (_state finalGame)

-- Bird tests
testSetBirdVy :: Test
testSetBirdVy = TestCase $ do
    let initialBird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let newBird = setBirdVy initialBird 10.0
    assertEqual "_birdVy should be updated" 10.0 (_birdVy newBird)

testBirdKill :: Test
testBirdKill = TestCase $ do
    let initialBird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let newBird = birdKill initialBird
    assertEqual "_dead should be True" True (_dead newBird)

testUpdateBirdY :: Test
testUpdateBirdY = TestCase $ do
    let initialBird = Bird { _birdX = 0, _birdY = 0, _birdVy = 10.0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let newBird = updateBirdY initialBird
    -- Assuming __dFps is 60.0, _birdY should decrease by 10.0 * (1.0 / 60.0) = 0.166...
    assertApproxEqual "_birdY should be updated" (_birdY initialBird - (_birdVy initialBird / __dFps)) (_birdY newBird) 0.0000001

testUpdateBirdVy :: Test
testUpdateBirdVy = TestCase $ do
    let initialBird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let newBird = updateBirdVy initialBird
    -- Assuming __gravity is 2000 and __dFps is 60.0, _birdVy should increase by 2000 * (1.0 / 60.0) = 33.333...
    assertApproxEqual "_birdVy should be updated" (__gravity / __dFps) (_birdVy newBird) 0.0000001

testBirdFalling :: Test
testBirdFalling = TestCase $ do
    let initialBird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let newBird = birdFalling initialBird
    -- _birdVy should increase and _birdY should decrease
    assertBool "_birdVy should increase" (_birdVy newBird > 0)
    assertBool "_birdY should decrease" (_birdY newBird < 0)

testUpdateCount :: Test
testUpdateCount = TestCase $ do
    let initialBird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let newBird = updateCount initialBird
    assertEqual "_count should increment" 1 (_count newBird)

testUpdatePicIndex :: Test
testUpdatePicIndex = TestCase $ do
    let initialBird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let newBird = updatePicIndex initialBird
    assertEqual "_pIndex should increment" 1 (_pIndex newBird)

testUpdateAngle :: Test
testUpdateAngle = TestCase $ do
    let initialBird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let newBird = updateAngle initialBird
    -- Assuming calcurateAngle returns 0 for _birdVy = 0
    assertEqual "_angle should be updated" 0 (_angle newBird)

testCalcurateAngle :: Test
testCalcurateAngle = TestCase $ do
    assertEqual "Should return 0 for 0 velocity" 0 (calcurateAngle 0)
    -- Assuming __maxBirdAngle = 90, __minBirdAngle = -45, __angleBias = 10.0
    assertEqual "Should return max angle for high velocity" __maxBirdAngle (calcurateAngle 1000.0)
    assertEqual "Should return min angle for low velocity" __minBirdAngle (calcurateAngle (-1000.0))

-- Pipe tests
testPipeUpdate :: Test
testPipeUpdate = TestCase $ do
    let initialPipe = Pipe { _pipeUp = 100, _pipeDw = 0, _pipeX = 0 }
    let newPipe = pipeUpdate initialPipe
    -- Assuming __pipeSpeed is -150.0 and __dFps is 60.0, _pipeX should decrease by -150.0 / 60.0 = -2.5
    assertApproxEqual "_pipeX should be updated" (-2.5) (_pipeX newPipe) 0.0000001

testPipeReset :: Test
testPipeReset = TestCase $ do
    let initialPipe = Pipe { _pipeUp = 100, _pipeDw = 0, _pipeX = -100 }
    let newPipe = pipeReset initialPipe 50 200
    assertEqual "_pipeX should be reset" 50 (_pipeX newPipe)
    assertEqual "_pipeUp should be reset" 200 (_pipeUp newPipe)
    assertEqual "_pipeDw should be reset" (200 + __pipesGap) (_pipeDw newPipe)

testPipesCollision :: Test
testPipesCollision = TestCase $ do
    let bird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let pipe1 = Pipe { _pipeUp = 10, _pipeDw = -10, _pipeX = 0 }
    let pipes = Pipes [pipe1]
    -- Bird is at (0,0), pipe is at (0,0) with gap from -10 to 10. Bird should collide.
    assertEqual "Should detect collision when bird is inside pipe" True (pipesCollision pipes bird)

testInsidePipesGap :: Test
testInsidePipesGap = TestCase $ do
    let bird = Bird { _birdX = 0, _birdY = 0, _birdVy = 0, _count = 0, _pIndex = 0, _angle = 0, _dead = False }
    let pipe1 = Pipe { _pipeUp = 100, _pipeDw = 0, _pipeX = 0 }
    let pipes = Pipes [pipe1]
    -- Bird is at (0,0), pipe is at (0,0) with gap from 0 to 100. Bird should be inside gap.
    assertEqual "Should detect bird inside pipe gap" True (insidePipesGap pipes bird)

-- A placeholder for all tests
allTests :: Test
allTests = TestList
    [ TestLabel "testAddScore" testAddScore
    , TestLabel "testUpdateScore" testUpdateScore
    , TestLabel "testParseHighScore" testParseHighScore
    , TestLabel "testFormatHighScore" testFormatHighScore
    , TestLabel "testGameInit" testGameInit
    , TestLabel "testEventHandler_GameStart_to_GameStop" testEventHandler_GameStart_to_GameStop
    , TestLabel "testSetBirdVy" testSetBirdVy
    , TestLabel "testBirdKill" testBirdKill
    , TestLabel "testUpdateBirdY" testUpdateBirdY
    , TestLabel "testUpdateBirdVy" testUpdateBirdVy
    , TestLabel "testBirdFalling" testBirdFalling
    , TestLabel "testUpdateCount" testUpdateCount
    , TestLabel "testUpdatePicIndex" testUpdatePicIndex
    , TestLabel "testUpdateAngle" testUpdateAngle
    , TestLabel "testCalcurateAngle" testCalcurateAngle
    , TestLabel "testPipeUpdate" testPipeUpdate
    , TestLabel "testPipeReset" testPipeReset
    , TestLabel "testPipesCollision" testPipesCollision
    , TestLabel "testInsidePipesGap" testInsidePipesGap
    ]

main :: IO Counts
main = runTestTT allTests
