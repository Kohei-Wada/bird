{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE BangPatterns #-}
module Ground (Ground(..), groundCollision) where

import Options
import Utils
import Actor
import Bird 

import Control.Monad
import Control.Monad.ST
import Data.STRef

data Ground = Ground 
    { _groundX   :: !Float 
    , _groundY   :: !Float
    , _groundWid :: !Int
    }

instance Actor Ground where
    initialize  = groundInit
    update      = groundUpdate
    onCollision = pure 

groundInit :: IO Ground
groundInit = let !w = __groundWid__ * expansionRate __groundWid__ 
              in pure Ground 
                  { _groundX   = -fromIntegral w 
                  , _groundY   = __defaultGroundY 
                  , _groundWid = w
                  }

groundUpdate :: Ground -> IO Ground
groundUpdate g = stToIO $ do 
    g' <- newSTRef g
    modifySTRef g' updateGroundX
    readSTRef g'

    where
        updateGroundX :: Ground -> Ground 
        updateGroundX g@Ground{..} = runST $ do 
            g' <- newSTRef g
            modifySTRef g' $ \g -> if _groundX < -(fromIntegral __wWidth) then resetGroundX g else addGroundX g
            readSTRef g'

        addGroundX :: Ground -> Ground 
        addGroundX g@Ground{..} = runST $ do 
            g' <- newSTRef g
            modifySTRef g' $ \g -> g { _groundX = _groundX + __groundSpeed * (1.0 / __fFps) }
            readSTRef g'

        resetGroundX :: Ground -> Ground
        resetGroundX g@Ground{..} = runST $ do 
            g' <- newSTRef g
            modifySTRef g' $ \g -> g { _groundX = -fromIntegral __groundWid + __groundResetBias } 
            readSTRef g'

groundCollision :: Ground -> Bird -> Bool
groundCollision Ground{..} b@Bird{..} = realToFrac _birdY < _groundY + __groundCollisionBias
