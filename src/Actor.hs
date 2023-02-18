module Actor where

import Prelude 

class Actor a where
    update :: a -> IO a
