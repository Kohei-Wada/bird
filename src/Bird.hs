{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE BangPatterns #-}

module Bird (Bird(..), birdFlapping, birdKill, birdSwooping) where
    
import Options
import Actor

import Control.Monad
import Control.Monad.ST
import Data.STRef
import Graphics.Gloss

data Bird = Bird 
    { _birdX    :: !Double
    , _birdY    :: !Double
    , _birdVy   :: !Float     
    , _count    :: !Int       -- count for FPS
    , _pIndex   :: !Int       -- Picture Index
    , _angle    :: !Float
    , _dead     :: !Bool
    } 

instance Actor Bird where
    initialize  = birdInit
    update      = birdUpdate
    onCollision = pure . birdKill


birdInit :: IO Bird
birdInit = pure Bird 
        { _birdX    = __birdX
        , _birdY    = __birdY 
        , _birdVy   = 0 
        , _count    = 0
        , _pIndex   = 0
        , _angle    = 0
        , _dead     = False
        }

setBirdVy :: Bird -> Float -> Bird
setBirdVy b vy = runST $ do 
    b' <- newSTRef b
    modifySTRef b' $ \b -> b { _birdVy = vy }
    readSTRef b'


birdKill :: Bird -> Bird
birdKill b = runST $ do 
    b' <- newSTRef b
    modifySTRef b' $ \b -> b { _dead = True }
    readSTRef b'


updateBirdY :: Bird -> Bird
updateBirdY b = runST $ do 
    b' <- newSTRef b
    modifySTRef b' $ 
        \b@Bird{..} -> b { _birdY = _birdY - realToFrac (_birdVy * (1.0 / __fFps)) } 
    readSTRef b'


updateBirdVy :: Bird -> Bird
updateBirdVy b = runST $ do 
    b' <- newSTRef b
    modifySTRef b' $ \b@Bird{..} -> b { _birdVy = _birdVy + __gravity * (1.0 / __fFps) }
    readSTRef b'

    
birdFalling :: Bird -> Bird   
birdFalling b = runST $ do 
    b' <- newSTRef b
    modifySTRef b' (updateBirdY . updateBirdVy) 
    readSTRef b'


birdFlapping :: Bird -> Bird
birdFlapping b@Bird{..} = runST $ do 
    b' <- newSTRef b
    modifySTRef b' $ \b -> if _dead then b else setBirdVy b __birdFlappingV 
    readSTRef b'


birdSwooping :: Bird -> Bird
birdSwooping b@Bird{..} = runST $ do 
    b' <- newSTRef b
    modifySTRef b' $ \b -> if _dead then b else setBirdVy b __birdSwoopingV
    readSTRef b'

    
birdUpdate :: Bird -> IO Bird 
birdUpdate b = stToIO $ do 
    b' <- newSTRef b
    modifySTRef b' birdUpdate'
    readSTRef b'

    where
        birdUpdate' :: Bird -> Bird
        birdUpdate' b@Bird{..} = 
            if _dead then (updateAngle . birdFalling) b
                     else (updateAngle . updatePicIndex . updateCount . birdFalling) b


updateCount :: Bird -> Bird
updateCount b@Bird{..} = runST $ do
    b' <- newSTRef b 
    modifySTRef b' $ \b -> b { _count = if (fromIntegral _count) >= (__fFps / __fPps) then 0 else _count + 1 }
    readSTRef b'


updatePicIndex :: Bird -> Bird
updatePicIndex b@Bird{..} = runST $ do 
    b' <- newSTRef b
    let !i = if _count == 0 then 
                if _pIndex == (__nBirdAssets - 1) then 0 else _pIndex + 1
            else 
                _pIndex

    modifySTRef b' $ \b -> b { _pIndex = i}
    readSTRef b'


updateAngle :: Bird -> Bird 
updateAngle b = runST $ do 
    b' <- newSTRef b
    modifySTRef b' (\b@Bird{..} -> b { _angle = calcurateAngle _birdVy })
    readSTRef b'


calcurateAngle :: Float -> Float
calcurateAngle vy 
  | tmp > __maxBirdAngle = __maxBirdAngle 
  | tmp < __minBirdAngle = __minBirdAngle
  | otherwise = tmp 
  where tmp = velocityToAngle vy


velocityToAngle :: Float -> Float
velocityToAngle v = v / __angleBias 
