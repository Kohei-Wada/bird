module Actor where

import Prelude 

class Actor a where
    initialize :: IO a
    update     :: a -> IO a
