{-#LANGUAGE RecordWildCards #-}
module Ground where

import Options
import Utils


data Ground = Ground 
    { _groundX   :: !Float 
    , _groundY   :: !Float
    , _groundWid :: !Int
    }


groundInit :: Ground
groundInit = let w = __groundWid__ * expansionRate __groundWid__ 
              in Ground 
                  { _groundX   = -fromIntegral w 
                  , _groundY   = __defaultGroundY 
                  , _groundWid = w
                  }


groundUpdate :: Ground -> Ground
groundUpdate =  updateGroundX


updateGroundX :: Ground -> Ground 
updateGroundX g@Ground{..} =  
    if _groundX < -(fromIntegral __wWidth) then resetGroundX g else addGroundX g


addGroundX :: Ground -> Ground 
addGroundX g@Ground{..} = g { _groundX = _groundX + __groundSpeed * (1.0 / __fFps) }


resetGroundX :: Ground -> Ground
resetGroundX g@Ground{..} = g { _groundX = -fromIntegral __groundWid } 


groundCollision :: Ground -> Float -> Float -> Bool
groundCollision g@Ground{..} x y = y < _groundY + __groundCollisionBias  

