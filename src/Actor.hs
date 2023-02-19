module Actor where

class Actor a where
    initialize  :: IO a
    update      :: a -> IO a
    onCollision :: a -> IO a
