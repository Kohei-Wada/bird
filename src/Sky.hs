{-#LANGUAGE RecordWildCards #-}
{-#LANGUAGE BangPatterns #-}
module Sky (Sky(..)) where

import Options
import Utils
import Actor

import Control.Monad
import Control.Monad.ST
import Data.STRef

data Sky = Sky 
    { _skyX   :: !Float 
    , _skyY   :: !Float
    , _skyWid :: !Int
    }

instance Actor Sky where
    initialize  = skyInit
    update      = skyUpdate
    onCollision = pure 

skyInit :: IO Sky
skyInit = let !w = __skyWid__ * expansionRate __skyWid__  
           in pure Sky 
               { _skyX   = -fromIntegral w 
               , _skyY   = __defaultSkyY 
               , _skyWid = w 
               }

skyUpdate :: Sky -> IO Sky
skyUpdate s = stToIO $ do 
    s' <- newSTRef s
    modifySTRef s' updateSkyX
    readSTRef s'


updateSkyX :: Sky -> Sky
updateSkyX s@Sky{..} = 
    s { _skyX  = if _skyX <  -(fromIntegral __wWidth) then -fromIntegral __skyWid else _skyX + __skySpeed * realToFrac (1.0/__fFps) 
      }
